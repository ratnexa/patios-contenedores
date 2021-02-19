import cv2
from local_utils import detect_lp
from os.path import splitext, basename
from keras.models import model_from_json
from sklearn.preprocessing import LabelEncoder
import glob
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import numpy as np
import datetime
import pandas as pd


def load_model(path):
    try:
        path = splitext(path)[0]
        with open('%s.json' % path, 'r') as json_file:
            model_json = json_file.read()
        model = model_from_json(model_json, custom_objects={})
        model.load_weights('%s.h5' % path)
        print("Loading model successfully...")
        return model
    except Exception as e:
        print(e)


def preprocess_image(img, resize=False):
    #img = cv2.imread(image_path)
    img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
    img = img / 255
    if resize:
        img = cv2.resize(img, (224, 224))
    return img


def get_plate(image, Dmax=608, Dmin=256):
    vehicle = preprocess_image(image)
    ratio = float(max(vehicle.shape[:2])) / min(vehicle.shape[:2])
    side = int(ratio * Dmin)
    bound_dim = min(side, Dmax)
    _, LpImg, _, cor = detect_lp(wpod_net, vehicle, bound_dim, lp_threshold=0.5)
    return vehicle, LpImg, cor


def shadow_remove(img):
    img = (img * 255).astype(np.uint8)
    rgb_planes = cv2.split(img)
    result_planes = []
    result_norm_planes = []
    for plane in rgb_planes:
        dilated_img = cv2.dilate(plane, np.ones((7, 7), np.uint8))
        bg_img = cv2.medianBlur(dilated_img, 21)
        diff_img = 255 - cv2.absdiff(plane, bg_img)
        norm_img = cv2.normalize(diff_img, None, alpha=0, beta=255, norm_type=cv2.NORM_MINMAX, dtype=cv2.CV_8UC1)
        result_planes.append(diff_img)
        result_norm_planes.append(norm_img)
    result = cv2.merge(result_planes)
    result_norm = cv2.merge(result_norm_planes)
    return result_norm

def sort_contours(cnts, reverse=False):
    i = 0
    boundingBoxes = [cv2.boundingRect(c) for c in cnts]
    (cnts, boundingBoxes) = zip(*sorted(zip(cnts, boundingBoxes),
                                        key=lambda b: b[1][i], reverse=reverse))
    return cnts

def predict_from_model(image, model, labels):
    image = cv2.resize(image, (80, 80))
    image = np.stack((image,) * 3, axis=-1)
    prediction = labels.inverse_transform([np.argmax(model.predict(image[np.newaxis, :]))])
    return prediction

def predict_plate_rmuv(LpImg):
    if len(LpImg):  # check if there is at least one license image

        # Shadow removal
        shad = shadow_remove(LpImg[0])
        # cv2.imwrite('after_shadow_remove1.jpg', shad)

        # Scales, calculates absolute values, and converts the result to 8-bit.
        plate_image = cv2.convertScaleAbs(LpImg[0], alpha=255.0)
        # cv2.imwrite("normal.jpg", plate_image)

        # convert to grayscale and blur the image
        gray = cv2.cvtColor(plate_image, cv2.COLOR_BGR2GRAY)
        gray_shad = cv2.cvtColor(shad, cv2.COLOR_BGR2GRAY)
        # cv2.imwrite("graytest.jpg", gray)
        blur = cv2.GaussianBlur(gray, (7, 7), 0)
        blur_shad = cv2.GaussianBlur(gray_shad, (7, 7), 0)

        # Applied inversed thresh_binary
        binary = cv2.threshold(blur, 180, 255,
                               cv2.THRESH_BINARY_INV + cv2.THRESH_OTSU)[1]
        binary_shad = cv2.threshold(blur_shad, 180, 255,
                                    cv2.THRESH_BINARY_INV + cv2.THRESH_OTSU)[1]
        # cv2.imwrite("binarytest.jpg", binary)

        kernel3 = cv2.getStructuringElement(cv2.MORPH_RECT, (3, 3))
        thre_mor = cv2.morphologyEx(binary, cv2.MORPH_DILATE, kernel3)
        thre_mor_shad = cv2.morphologyEx(binary_shad, cv2.MORPH_DILATE, kernel3)
        # cv2.imwrite("kernel.jpg", thre_mor)

    image_type = [binary_shad, binary]
    final_crop_characters = []
    sizes = []
    detected_letters = 0
    for k in image_type:

        cont, _ = cv2.findContours(k, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        # creat a copy version "test_roi" of plat_image to draw bounding box
        test_roi = plate_image.copy()

        # Initialize a list which will be used to append charater image
        crop_characters = []

        # define standard width and height of character
        digit_w, digit_h = 30, 60
        counter = 0

        for c in sort_contours(cont):
            (x, y, w, h) = cv2.boundingRect(c)
            ratio = h / w
            if 3 >= ratio >= 0.8:  # Only select contour with defined ratio
                if h / plate_image.shape[0] >= 0.4:  # Select contour which has the height larger than 50% of the plate
                    if w / plate_image.shape[1] <= 0.2:
                        cv2.rectangle(test_roi, (x, y), (x + w, y + h), (0, 255, 0), 2)

                        # Sperate number and give prediction
                        curr_num = thre_mor[y:y + h, x:x + w]
                        curr_num = cv2.resize(curr_num, dsize=(digit_w, digit_h))
                        _, curr_num = cv2.threshold(curr_num, 220, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)
                        crop_characters.append(curr_num)
            counter += 1

        #print("Detect {} letters...".format(len(crop_characters)))
        if detected_letters < len(crop_characters):
            detected_letters = len(crop_characters)
            final_crop_characters = crop_characters

    len(final_crop_characters)
    crop_characters = final_crop_characters
    final_string = ''
    for i, character in enumerate(crop_characters):
        title = np.array2string(predict_from_model(character, model, labels))
        final_string += title.strip("'[]")
    return(final_string, crop_characters)

json_file = open('MobileNets_character_recognition.json', 'r')
loaded_model_json = json_file.read()
json_file.close()
model = model_from_json(loaded_model_json)
model.load_weights("License_character_recognition_weight.h5")
print("[INFO] Model loaded successfully...")

labels = LabelEncoder()
labels.classes_ = np.load('license_character_classes.npy')
print("[INFO] Labels loaded successfully...")

wpod_net_path = "wpod-net.json"
wpod_net = load_model(wpod_net_path)
vid = "videosTest/videotest2.mp4"
cap = cv2.VideoCapture(1)


#cap.set(cv2.CAP_PROP_BUFFERSIZE, 3)
#test_image_path = "test1/testReal.png"
#vehicle, LpImg, cor = get_plate(test_image_path)

if not cap.isOpened():
    print("Error Opening Video")
counter = 0
LpImg = []
t1 = datetime.datetime.now()
bdDataFrame = pd.DataFrame({"plate": [],
        "register_date": [],
        "camera_id": []})
while True:
    ret, frame = cap.read()
    counter += 1

    # if ret == True:
    # draw = ImageDraw.Draw(frame)
    #vehicle, LpImg, cor = get_plate(test_image_path
    cv2.imshow("Frame", frame)

    #cv2.imwrite('frame.jpg', frame)
    if (counter % 30) == 0:
        try:
            vehicle, LpImg, cor = get_plate(frame)

        except Exception as e:
            LpImg = []

        if len(LpImg) > 0:
            plateString, characters = predict_plate_rmuv(LpImg)
            print(plateString)
            t2 = datetime.datetime.now()
            formatDate = t2.strftime("%Y-%m-%d %H:%M:%S")
            newRow = pd.DataFrame({
                "plate": [plateString],
                "register_date": [formatDate],
                "camera_id": [1]
            })

            bdDataFrame = bdDataFrame.append(newRow)
            cv2.imshow("Frame", LpImg[0])





    if cv2.waitKey(1) & 0xFF == ord('q'):
        break
    # else:
    #    break


pd.DataFrame.to_csv(bdDataFrame, "bd_test.csv", sep = ";", index = False)
cap.release()
cv2.destroyAllWindows()



delta = t2 - t1
print(delta.total_seconds())



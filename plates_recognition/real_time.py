import cv2
from local_utils import detect_lp
from os.path import splitext, basename
from keras.models import model_from_json
from sklearn.preprocessing import LabelEncoder
import glob
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import numpy as np


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


def preprocess_image(image_path, resize=False):
    img = cv2.imread(image_path)
    img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
    img = img / 255
    if resize:
        img = cv2.resize(img, (224, 224))
    return img


def get_plate(image_path, Dmax=608, Dmin=256):
    vehicle = preprocess_image(image_path)
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
    cv2.imwrite('shadows_out.png', result)
    cv2.imwrite('shadows_out_norm.png', result_norm)
    return result_norm


wpod_net_path = "wpod-net.json"
wpod_net = load_model(wpod_net_path)

cap = cv2.VideoCapture(0)
test_image_path = "test1/foto4.jpg"
vehicle, LpImg, cor = get_plate(test_image_path)

if not cap.isOpened():
    print("Error Opening Video")

while True:
    ret, frame = cap.read()
    # if ret == True:
    # draw = ImageDraw.Draw(frame)
    #vehicle, LpImg, cor = get_plate(test_image_path)
    cv2.imshow("Frame", frame)

    cv2.imshow("Frame", LpImg[0])


    if cv2.waitKey(1) & 0xFF == ord('q'):
        break
    # else:
    #    break
cap.release()

cv2.destroyAllWindows()

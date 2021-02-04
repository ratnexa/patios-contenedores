# Prior the implementation of character segmentation, we must clean
# our data first
# Convert to 255 the extracted plate
# Convert to grayscale (this improves computational time)
# Blur image to get out the noise of the picture (uses gaussian blur), (7,7), depending on the kernel sizem, the nmore noise is removed but the more info is lost
# Image thresholding dont know wtf it is
# Dilation is a technique to increase the white region of the image

import cv2
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
from local_utils import detect_lp
from os.path import splitext, basename
from keras.models import model_from_json
import glob


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


wpod_net_path = "wpod-net.json"
wpod_net = load_model(wpod_net_path)


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


test_image_path = "test1/test8.jpeg"

vehicle, LpImg, cor = get_plate(test_image_path)

fig = plt.figure(figsize=(12, 6))
grid = gridspec.GridSpec(ncols=2, nrows=1, figure=fig)
fig.add_subplot(grid[0])
plt.axis(False)
plt.imshow(vehicle)
grid = gridspec.GridSpec(ncols=2, nrows=1, figure=fig)
fig.add_subplot(grid[1])
plt.axis(False)
plt.imshow(LpImg[0])
plt.show()

if len(LpImg):  # check if there is at least one license image

    # Shadow removal
    shad = shadow_remove(LpImg[0])
    cv2.imwrite('after_shadow_remove1.jpg', shad)

    # Scales, calculates absolute values, and converts the result to 8-bit.
    plate_image = cv2.convertScaleAbs(LpImg[0], alpha=255.0)
    cv2.imwrite("normal.jpg", plate_image)

    # convert to grayscale and blur the image
    gray = cv2.cvtColor(shad, cv2.COLOR_BGR2GRAY)
    cv2.imwrite("graytest.jpg", gray)
    blur = cv2.GaussianBlur(gray, (7, 7), 0)

    # Applied inversed thresh_binary
    binary = cv2.threshold(blur, 180, 255,
                           cv2.THRESH_BINARY_INV + cv2.THRESH_OTSU)[1]
    cv2.imwrite("binarytest.jpg", binary)

    kernel3 = cv2.getStructuringElement(cv2.MORPH_RECT, (3, 3))
    thre_mor = cv2.morphologyEx(binary, cv2.MORPH_DILATE, kernel3)
    cv2.imwrite("kernel.jpg", thre_mor)

# visualize results
fig = plt.figure(figsize=(12, 7))
plt.rcParams.update({"font.size": 18})
grid = gridspec.GridSpec(ncols=2, nrows=3, figure=fig)
plot_image = [shad, plate_image, gray, blur, binary, thre_mor]
plot_name = ["shad", "plate_image", "gray", "blur", "binary", "dilation"]

for i in range(len(plot_image)):
    fig.add_subplot(grid[i])
    plt.axis(False)
    plt.title(plot_name[i])
    if i == 0:
        plt.imshow(plot_image[i])
    else:
        plt.imshow(plot_image[i], cmap="gray")

plt.show()


def sort_contours(cnts, reverse=False):
    i = 0
    boundingBoxes = [cv2.boundingRect(c) for c in cnts]
    (cnts, boundingBoxes) = zip(*sorted(zip(cnts, boundingBoxes),
                                        key=lambda b: b[1][i], reverse=reverse))
    return cnts


cont, _ = cv2.findContours(binary, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)

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
    print(str(counter) + ", Height " + str(h) + ", Width " + str(w) + ", Ratio " + str(h / w))
    if 3 >= ratio >= 0.8:  # Only select contour with defined ratio
        if h / plate_image.shape[0] >= 0.4:  # Select contour which has the height larger than 50% of the plate
            # if h / plate_image.shape[0] <= 0.7:
            # if w / plate_image.shape[0] <= 0.5:
            # Draw bounding box arroung digit number
            print(str(counter) + ", Height " + str(h) + ", Width " + str(w) + ", Ratio " + str(h / w))
            cv2.rectangle(test_roi, (x, y), (x + w, y + h), (0, 255, 0), 2)

            # Sperate number and gibe prediction
            curr_num = thre_mor[y:y + h, x:x + w]
            curr_num = cv2.resize(curr_num, dsize=(digit_w, digit_h))
            _, curr_num = cv2.threshold(curr_num, 220, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)
            crop_characters.append(curr_num)
    counter += 1

print("Detect {} letters...".format(len(crop_characters)))
fig = plt.figure(figsize=(10, 6))
plt.axis(False)
plt.imshow(test_roi)
plt.show()

fig = plt.figure(figsize=(14, 4))
grid = gridspec.GridSpec(ncols=len(crop_characters), nrows=1, figure=fig)

for i in range(len(crop_characters)):
    fig.add_subplot(grid[i])
    plt.axis(False)
    plt.imshow(crop_characters[i], cmap="gray")
plt.show()

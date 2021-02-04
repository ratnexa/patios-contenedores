import cv2
import numpy as np
import matplotlib.pyplot as plt
from local_utils import detect_lp
from os.path import splitext, basename
from keras.models import model_from_json
import glob


# Load pre trained model
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


# Preprocess the image into some readable characters
def preprocess_image(image_path, resize=False):
    img = cv2.imread(image_path)
    img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
    img = img / 255
    if resize:
        img = cv2.resize(img, (224, 224))
    return img


# Create a list of image paths
image_paths = glob.glob("Plate_examples/*.jpg")
print("Found %i images..." % (len(image_paths)))

fig = plt.figure(figsize=(12, 8))
cols = 5
rows = 5
fig_list = []
for i in range(cols * rows):
    fig_list.append(fig.add_subplot(rows, cols, i + 1))
    title = splitext(basename(image_paths[i]))[0]
    fig_list[-1].set_title(title)
    img = preprocess_image(image_paths[i], True)
    plt.axis(False)
    plt.imshow(img)

plt.tight_layout(True)
plt.show()


# Function to get the plate
def get_plate(image_path, Dmax=608, Dmin=256):
    vehicle = preprocess_image(image_path)
    ratio = float(max(vehicle.shape[:2])) / min(vehicle.shape[:2])
    side = int(ratio * Dmin)
    bound_dim = min(side, Dmax)
    _, LpImg, _, cor = detect_lp(wpod_net, vehicle, bound_dim, lp_threshold=0.5)
    return LpImg, cor


# Obtain plate image and its coordinates from an image
test_image = image_paths[12]
LpImg, cor = get_plate(test_image)
print("Detect %i plate(s) in" % len(LpImg), splitext(basename(test_image))[0])
print("Coordinate of plate(s) in image: \n", cor)

plt.figure(figsize=(10, 5))
plt.subplot(1, 2, 1)
plt.axis(False)
plt.imshow(preprocess_image(test_image))
plt.subplot(1, 2, 2)
plt.axis(False)
plt.imshow(LpImg[0])
plt.show()


# This draws a box on the plate
def draw_box(image_path, cor, thickness=3):
    pts = []
    x_coordinates = cor[0][0]
    y_coordinates = cor[0][1]
    # store the top-left, top-right, bottom-left, bottom-right
    # of the plate license respectively
    for i in range(4):
        pts.append([int(x_coordinates[i]), int(y_coordinates[i])])

    pts = np.array(pts, np.int32)
    pts = pts.reshape((-1, 1, 2))
    vehicle_image = preprocess_image(image_path)

    cv2.polylines(vehicle_image, [pts], True, (0, 255, 0), thickness)
    return vehicle_image


plt.figure(figsize=(8, 8))
plt.axis(False)
plt.imshow(draw_box(test_image, cor))
plt.show()

# Viualize all obtained plate images
fig = plt.figure(figsize=(12, 6))
cols = 5
rows = 5
fig_list = []

for i in range(cols * rows):
    fig_list.append(fig.add_subplot(rows, cols, i + 1))
    title = splitext(basename(image_paths[i]))[0]
    fig_list[-1].set_title(title)
    try:
        LpImg, _ = get_plate(image_paths[i])
    except:
        next
    plt.axis(False)
    plt.imshow(LpImg[0])

plt.tight_layout(True)
plt.show()

# Detect multiple plates
multiple_plates_image = "Plate_examples/multiple_plates.png"
LpImg, cor = get_plate(multiple_plates_image)

print("Detect %i plate(s) in" % len(LpImg), splitext(basename(multiple_plates_image))[0])

# Visualize the original image
plt.figure(figsize=(10, 5))
plt.axis(False)
plt.imshow(preprocess_image(multiple_plates_image))

# Visualize the obtained plates
plt.figure(figsize=(10, 5))
plt.subplot(1, 2, 1)
plt.axis(False)
plt.imshow(LpImg[1])
plt.subplot(1, 2, 2)
plt.axis(False)
plt.imshow(LpImg[0])
plt.show()

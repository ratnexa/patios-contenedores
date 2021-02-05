
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
            # Draw bounding box around digit number
            print(str(counter) + ", Height " + str(h) + ", Width " + str(w) + ", Ratio " + str(h / w))
            cv2.rectangle(test_roi, (x, y), (x + w, y + h), (0, 255, 0), 2)

            # Sperate number and give prediction
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

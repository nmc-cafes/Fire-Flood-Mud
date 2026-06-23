# import numpy as np

# # Create a NumPy array (replace this with your array)
# arr = np.array([[2, 1, 2, 3, 5, 5],
#                 [1, 4, 4, 3, 5, 5],
#                 [4, 4, 4, 5, 4, 4],
#                 [6, 6, 6, 7, 3, 4],
#                 [6, 6, 6, 7, 3, 7],
#                 [6, 6, 6, 7, 3, 7]])

# def find_identical_subsection(arr, A, B):
#     rows, cols = arr.shape
#     xa,ya =  (0,0)
#     xd,yd = (0,0)
#     equal = False
#     while equal == False:
#         for r in range(xa,rows - A + 1):
#             for c in range(ya, cols - B + 1):
#                 # Extract the AxB subsection
#                 subsection = arr[r:r+A, c:c+B]
                
#                 # Check if all values in the subsection are identical
#                 if np.all(subsection == subsection[0, 0]):
#                     tla = (r, c)  # Return the top-left coordinates of the first identical subsection
#         for c in range(xd,cols - A + 1):
#             for r in range(yd, rows - B + 1):
#                 # Extract the AxB subsection
#                 subsection = arr[r:r+A, c:c+B]
                
#                 # Check if all values in the subsection are identical
#                 if np.all(subsection == subsection[0, 0]):
#                     tld = (r, c)  # Return the top-left coordinates of the first identical subsection
#         if tla == tld:
#             equal = True
#         else:


# A, B = 2, 2  # Define the dimensions of the subsection

# top_left_coords = find_identical_subsection(arr, A, B)

# print("Top-left coordinates of the first identical {}x{} subsection:".format(A,B), top_left_coords)

# arr2 = arr[top_left_coords[0]:,top_left_coords[1]:]
# print(arr2)

import numpy as np

# Create the NumPy array
arr = np.array([[1, 2, 2, 4, 4, 6],
                [2, 2, 2, 3, 3, 7],
                [2, 2, 2, 3, 3, 7],
                [4, 5, 5, 4, 4, 2],
                [4, 5, 5, 4, 4, 2],
                [1, 1, 1, 6, 6, 4]])

# Define the size of the 2x2 subsection
subsec_size = (2, 2)

def find_top_left_of_middle_subsection(arr, subsec_size):
    rows, cols = arr.shape

    # Calculate the bounds for the middle portion
    top = subsec_size[0]
    left = subsec_size[1]
    bottom = rows - subsec_size[0]
    right = cols - subsec_size[1]

    # Iterate through the middle portion
    for r in range(top, bottom):
        for c in range(left, right):
            # Extract the 2x2 subsection
            subsection = arr[r:r+2, c:c+2]

            # Check if all values in the subsection are identical
            if np.all(subsection == subsection[0, 0]):
                return (r, c)  # Return the top-left coordinates

    return None  # If no such subsection is found

top_left_coords = find_top_left_of_middle_subsection(arr, subsec_size)

if top_left_coords is not None:
    print("Top-left coordinates of the middle 2x2 subsection:", top_left_coords)
else:
    print("No middle 2x2 subsection found.")


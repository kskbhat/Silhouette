library(hexSticker)
sticker(
subplot = "image.png",            # your existing silhouette logo
s_x = 1,                          # center horizontally
s_y = 1,                       # position image vertically
s_width = 0.8,                    # adjust image size
s_height = 0.8,
package = "Silhouette",          # package name
p_size = 18,                     # text size (a bit larger)
p_color = "#005F9E",             # darker blue for better contrast
p_y = 1.55,                      # slightly higher than default
h_fill = "white",                # white background
h_color = "#007ACC",             # hexagon border color
filename = "logo.png", # output file
dpi = 300                        # high resolution
)

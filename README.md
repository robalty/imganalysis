# ImgAnalysis
Just a little haskell project to play around with the features of the language, and get better at using a pure funtional style. Takes as an argument the path to a .png file, converts it to RGB8, quantizes it to 16 levels per color channel, and then looks over all its pixels. After that, it generates an output image based on the number of pixels of each color and the approximate location in the image where the pixels of that color were found. The results are pretty abstract, but if
you  use an image with distinct enough blobs of color and REALLY squint, you can sort of see it.

Uses the JuicyPixels library by Vincent Berthoux. Thank you!

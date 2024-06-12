# Image Processing Project

##### Haskell program of basic ISP pipeline

This project implements basic image processing functionalities including brightness adjustment, contrast adjustment, gamma correction, convolution operations, and median filtering. (for RGB8 images)



## Features

- Brightness Adjustment
- Contrast Adjustment
- Gamma Correction
- Convolution (Gaussian Blur)
- Median Filtering



## Installation

### Using Cabal

First, ensure you have [Cabal](https://www.haskell.org/cabal/) installed. Then, navigate to your project directory and run:

```sh
cabal update
cabal install --only-dependencies
cabal build
```

### Using Stack
First, ensure you have Stack installed. Then, navigate to your project directory and run:

```sh
stack setup
stack build
```



## Usage

### Command Line
To run the project from the command line, use the following command:

```sh
cabal run image-processor
```
Or
```sh
make run
```
### Graphical User Interface
- Then you will see the Graphical User Interface.

- Run the application using one of the commands above.

- Load an image from the input directory by specifying the path.

- Select the desired processing options (e.g., brightness, contrast).

- Click "Apply Processing" to see the results.

- The processed image will be saved in the output directory.

  


## Example
### Input Image
- Place your input image in the input directory. Here is an example input image:

![](input/kodim15.png)

### Processed Image
- The processed image will be saved in the output directory. Here is an example output image:
> - Brightness -20
> - Contrast 1.4
> - Gamma 0.7
> - Use Convolution 
> - Use Median Filtering

![](output/output_image.png)



## Project Structure
```tree.log
code/
  ├── ImageProcessing/
  │   ├── Brightness.hs        # Handles brightness adjustment
  │   ├── Contrast.hs          # Handles contrast adjustment
  │   ├── Convolution.hs       # Handles convolution operations
  │   ├── Gamma.hs             # Handles gamma correction
  │   ├── Helpers.hs           # Contains helper functions
  │   ├── MedianFilter.hs      # Handles median filtering
  ├── src/
  │   ├── GUI.hs               # Handles the graphical user interface
  │   ├── Main.hs              # The main entry point of the application
  ├── input/
  │   └── kodim15.png          # Example input image
  ├── output/                  # Directory for processed images
  ├── image-processing.cabal   # Cabal configuration file
  ├── stack.yaml               # Stack configuration file
  ├── Makefile                 # Makefile for building the project
  ├── README.md                # This readme file
  ├── LICENSE                  # License file
  ├── .gitignore               # Git ignore file
```



## Authors

HeZexian, ChenJieyao - Fudan University



## Acknowledgements

- JuicyPixels: A library for loading and saving images in various formats.
- GTK: A GUI toolkit for Haskell.



## Contact

For any questions or feedback, please open an issue or contact the project maintainers.
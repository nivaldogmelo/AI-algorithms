# AI-algorithms

A couple of AI algorithms written with the help of the PAIP book

### Execution

1. Build the image

`cd gps && docker build . -t gps`

2. Start the image

`docker run --rm -ti gps:latest`

3. Initiate the code

`sbcl --load initial-version.lisp`

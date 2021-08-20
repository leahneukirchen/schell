all: schell glob/glob.so

glob/glob.so: glob/glob.c
	$(CC) -fPIC -shared -o $@ $^ -lchibi-scheme

glob/glob.c: glob/glob.stub
	chibi-ffi $^

schell: schell.img
	sed -e "s:@@CHIBI@@:$$(command -v chibi-scheme):g" \
            -e "s:@@IMAGE@@:$$PWD/schell.img:g" <schell.in >schell
	chmod +x schell

# NB: expanded -I is necessary for loading the image in other cwd
schell.img: FRC
	chibi-scheme -I $$PWD -x schell -d schell.img

FRC:

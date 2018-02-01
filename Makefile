# Take a look at: https://gist.github.com/halpo/1374344

PKG_VERSION=$(shell grep -i ^version ./DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package ./DESCRIPTION | cut -d : -d \  -f 2)

R_FILES := $(wildcard ./R/*.R)

windows: $(R_FILES)
	R --vanilla -e 'devtools::document(\".\")'
	Rcmd INSTALL --build .

clean:
	$(RM) $(PKG_NAME)_*.tar.gz

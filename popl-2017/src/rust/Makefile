TARGETS=const format map regexp vector

all: ${TARGETS}

%: %.rs
	rustc $<

clean:
	rm -f ${TARGETS}


SRC_DIR = src
LIB_DIR = lib
CP   	= ${LIB_DIR}:${LIB_DIR}/inflib.jar
OPTS 	= -Xlint:unchecked

JAVA_OPTS = -d ${LIB_DIR} -sourcepath ${SRC_DIR} -cp ${CP}

all:
	javac ${JAVA_OPTS} ${SRC_DIR}/*.java

debug:
	javac -g ${JAVA_OPTS} ${SRC_DIR}/*.java

clean:
	rm ${LIB_DIR}/*.class

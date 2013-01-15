CC=gcc
CFLAGS="-g -I/usr/local/erlang/R14B04/lib/erlang/lib/erl_interface-3.7.5/include -I/usr/local/include -Wall"
LDFLAGS="-L/usr/local/erlang/R14B04/lib/erlang/lib/erl_interface-3.7.5/lib -L/usr/local/lib -lopencv_core -lopencv_imgproc -lopencv_highgui -lerl_interface -lei -lpthread"

$CC $CFLAGS -o priv/resizerl c_src/erl_comm.c c_src/main.c $LDFLAGS

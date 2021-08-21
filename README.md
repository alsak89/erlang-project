# Final Project
# Functional Programming in Concurrent and Distributed Systems
# Ben Gurion University
# Spring 2021

# File Repository Load Balance Distributed System
This system is meant to act as a file container and manager which uses the resources of a few computers instead of only one.
Basic operations that are supported:
* Store file.
* Load file.
* Delete file.
* Update file.

Advanced operations that are supported:
* Show statistics on the memory usage of every machine.
* Upon closure of a node, its files are redistributed among other nodes using backup.

The system runs on Erlang OTP 22 on Ubuntu, and assumes wxwidgets is installed.

Steps for server initialization:
1. From the machine on which you wish to start the server, check the ip using ifconfig.
2. In the file server.erl, in line 43 instead of 127.0.0.1 write down the ip of the server maching.
3. Open Erlang on terminal while in the folder src. Make sure to set a cookie which is to be known accross all computers. For example: erlang -setcookie secret
4. Compile server.erl.
5. Run server:start_link().

Steps for client initialization: (can only be done after server is up):
1. From the machine on which you wish to start the server, check the ip using ifconfig.
2. Open Erlang on terminal while in the folder src. Make sure to name the terminal opened using the machine's ip and set a cookie which is to be known accross all computers. For example: erl -name 'a@127.0.0.1' -setcookie secret
3. Compile gui.erl.
4. Run net_adm:ping(ServerName), where ServerName is the same exact name as written in line 43 of server.erl.
5. Run gui:main(ServerName,SavedFileAddress), where ServerName is the same as in step 4, and SavedFileAddress is a string containing the path in which files are to be saved on the current machine.


# Link to YouTube video: https://youtu.be/3x7RwxibDC8

# Link to presentation(password is erl): https://easyupload.io/jt1ee6

Enjoy!

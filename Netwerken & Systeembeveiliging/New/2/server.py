"""
Lab 2 - HTTP Server
NAME: Steven Raaijmakers
STUDENT ID: 10804242
DESCRIPTION: Server thing
"""

# This is just an example of how you could implement the server. You may change
# this however you wish.
# For example, you could do a really nice object oriented version if you like.

import socket
import os
import subprocess
import mimetypes


# Create a list which incudes HTTP header and content
def create_http(message, content, ctype):
    k = []
    k.append("HTTP/1.1 " + message + " \r\n")
    k.append("Connection: close\r\n")
    k.append("Content-Type: " + ctype + "\r\n")
    k.append("Content-Length: " + str(len(content)) + "\r\n")
    k.append("Server: NginX\r\n\r\n")
    k.append(content)
    return k


# Check wheter given path is valid and if it belongs to cgibin or publichtml
def validate_path(path, publichtml, cgibin):
    file_type = 0
    new_path = ""
    if "cgi-bin" in path:
        new_path = cgibin + path.replace("/cgi-bin", "")
        file_type = 1
    else:
        new_path = public_html + path.split("?")[0]

    if not os.path.isfile(new_path.split("?")[0]):
        file_type = 2

    return new_path, file_type


# Server setup
def serve(port, public_html, cgibin):
    # Create server
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    s.bind(("localhost", port))
    s.listen(1)

    # Actions while server is running
    while True:
        c, _ = s.accept()
        data = c.recv(512)

        # Parse information from data
        method = data.split()[0]
        file_name = data.split()[1]

        path, file_type = validate_path(file_name, public_html, cgibin)

        # For a get method
        if method == "GET":
            if file_type != 2:  # when file IS found
                if file_type == 0:  # when file in public-html
                    file = open(path, 'r')
                    file_r = file.read()
                    ctype = str(mimetypes.guess_type(path))
                    for t in create_http("200 OK", file_r, ctype):
                        c.send(t)
                    file.close()
                elif file_type == 1:  # when cgibin file is called
                    uri = data.split()[1].replace("/cgi-bin/", "")
                    query_string = ""
                    if "?" in uri:
                        tmp = uri.split("?")
                        uri = tmp[0]
                        query_string = tmp[1]

                    # Set environs
                    os.environ['DOCUMENT_ROOT'] = public_html
                    os.environ['REQUEST_METHOD'] = "GET"
                    os.environ['REQUEST_URI'] = uri
                    os.environ['QUERY_STRING'] = query_string
                    os.environ['REMOTE_ADDR'] = c.getpeername()[0]

                    output = subprocess.check_output("python " +
                                                     "./cgi-bin/" + uri,
                                                     shell=True)
                    c.send(output)
            # when file not found
            else:
                message = "404 not found"
                for t in create_http(message, message, "text/html"):
                    c.send(t)
        # when the method is other than GET
        else:
            message = "501 not implemented"
            for t in create_http(message, message, "text/html"):
                c.send(t)
        c.close()
    s.close()

# This the entry point of the script.
# Do not change this part.
if __name__ == '__main__':
    import os
    import sys
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('--port', help='port to bind to', default=8080, type=int)
    p.add_argument('--public_html', help='home directory',
                   default='./public_html')
    p.add_argument('--cgibin', help='cgi-bin directory', default='./cgi-bin')
    args = p.parse_args(sys.argv[1:])
    public_html = os.path.abspath(args.public_html)
    cgibin = os.path.abspath(args.cgibin)
    serve(args.port, public_html, cgibin)

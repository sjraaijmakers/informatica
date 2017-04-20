from rest import app
import sys

if __name__ == '__main__':
    usage = "give args"

    if len(sys.argv) != 2:
        port = 8080
    else:
        port = int(sys.argv[1])
    print(port)
    app.run(port=port, debug=True)

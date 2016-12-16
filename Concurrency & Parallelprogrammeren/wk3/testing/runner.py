import subprocess

def callmapper(args):
    #pipe
    subprocess.Popen('hdfs dfs -rm -r output',stdout=subprocess.PIPE,shell=True)
    proc = subprocess.Popen('yarn jar target/wordcount-example-0.1-SNAPSHOT.jar nl.uva.cpp.WordCount  tweets2009-06-brg1of2.txt  output/ ' + str(args), stdout=subprocess.PIPE,shell=True)

    text = proc.communicate()[0].decode('utf-8')
    #write
    with open("runner_output.txt", "a") as myfile:
        myfile.write("args was: " + str(args))

    for line in (line for line in text if line.rstrip('\n')):
            with open("runner_output.txt", "a") as myfile:
                myfile.write(line)

    with open("runner_output.txt", "a") as myfile:
        myfile.write("\n")

    #clean
    with open('runner_output.txt', 'r') as file :
        filedata = file.read()

    myfile = filedata.replace(str(args)+ "OpenJDK Server VM warning: You have loaded library /home/ubuntu/Desktop/cpp/hathi-client/hadoop-2.7.1/lib/native/libhadoop.so.1.0.0 which might have disabled stack guard. The VM will try to fix the stack guard now.It's highly recommended that you fix the library with 'execstack -c <libfile>', or link it with '-z noexecstack'.Job", str(args) + " Job")

    with open('runner_output.txt', 'w') as file:
        file.write(myfile)

def main():
    #one
    callmapper(1)

    #two
    callmapper(2)

    #four
    callmapper(4)

    #eight
    callmapper(8)

if __name__ == '__main__':
    main()

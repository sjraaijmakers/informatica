package uva.nc.utils;

import android.os.Environment;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

/**
 * Created by bumbadadabum on 16-1-17.
 */

public class ReceivedFile {
    final public static int CHUNK_SIZE = 64 * 1024; // 64 kilobytes
    public static File RECEIVED_DIR = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES), "ncc");

    private File file;
    private long received;

    public int numChunks;

    public ReceivedFile(String fname, long received) throws FileNotFoundException {
        this.received = received;
        this.file = new File(RECEIVED_DIR, fname);
        if (this.file.exists()) {
            this.numChunks = (int) Math.ceil((double) this.file.length() / CHUNK_SIZE);
        } else if (received > 0) {
            throw new FileNotFoundException("File " + file.getPath() + " not found.");
        }
    }

    public void init(int size) {
        byte[] temp = new byte[size];
        this.numChunks = (int) Math.ceil((double) size / CHUNK_SIZE);

        try {
            FileOutputStream out = new FileOutputStream(this.file);
            out.write(temp);
        } catch (java.io.IOException e) {
            e.printStackTrace();
        }
    }

    public void writeChunk(int num, byte[] data) throws IOException {
        if (num < 0 || num > this.numChunks - 1) {
            throw new IndexOutOfBoundsException("Invalid number for chunk.");
        }

        FileOutputStream out = new FileOutputStream(this.file);
        try {
            FileChannel ch = out.getChannel();
            ch.position(num * CHUNK_SIZE);
            ch.write(ByteBuffer.wrap(data));
        } finally {
            out.close();
            this.received += Math.pow(2, num);
        }
    }

    public int getProgress() {
        String bitstring = Long.toBinaryString(this.received);

        int ones = 0, total = 0;

        // Hacky but it works
        for (int i = 0; i < bitstring.length(); i++) {
            if (bitstring.charAt(i) == '1') {
                ones++;
            }
            total++;
        }

        return (int) Math.round(((double) ones) / total * 100.0);
    }

    public File getFile() {
        return this.file;
    }

    public Tuple<String, Long> getFileInfo() {
        return new Tuple<String, Long>(this.file.getPath(), this.received);
    }
}

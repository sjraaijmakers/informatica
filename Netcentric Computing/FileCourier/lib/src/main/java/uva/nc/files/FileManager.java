package uva.nc.files;

import android.app.Activity;
import android.content.Context;
import android.os.Environment;
import android.widget.ArrayAdapter;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uva.nc.utils.JSONLoader;
import uva.nc.utils.ReceivedFile;
import uva.nc.utils.Tuple;

/**
 * Created by bumbadadabum on 11-1-17.
 */

public class FileManager {
    static final String OWN_FILELIST_FILENAME = "own-files.json";
    static final String RECEIVED_FILELIST_FILENAME = "received-files.json";
//    static final File FILE_DIR = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_PICTURES), "ncc");


    private File ownOutput;
    private File receivedOutput;
    private Map<String, Tuple<File, String>> fileList = new HashMap<String, Tuple<File, String>>();
    private Map<String, ReceivedFile> receivedList = new HashMap<String, ReceivedFile>();
    private Context context;

    public Map<String, String> destinationList = new HashMap<String, String>();

    public FileManager(Context context) {
        this.context = context;
        //if (!FILE_DIR.exists()) FILE_DIR.mkdir();

        this.ownOutput = new File(context.getFilesDir(), OWN_FILELIST_FILENAME);

        if (this.ownOutput.exists()) {
            try {
                JSONArray output = JSONLoader.loadJsonArray(this.ownOutput);

                for (int i = 0; i < output.length(); i++) {
                    JSONObject o = (JSONObject) output.get(i);

                    File f = new File(o.getString("path"));
                    if (f.exists()) {
                        this.fileList.put(o.getString("filename"), new Tuple<File, String>(f, o.getString("original-name")));
                        if (o.has("destination")) {
                            destinationList.put(o.getString("filename"), o.getString("destination"));
                        }
                    }
                }
            } catch (Exception e) {
                return;
            }
        }

        this.receivedOutput = new File(context.getFilesDir(), RECEIVED_FILELIST_FILENAME);

        if (this.receivedOutput.exists()) {
            try {
                JSONArray output = JSONLoader.loadJsonArray(this.receivedOutput);

                for (int i = 0; i < output.length(); i++) {
                    JSONObject o = (JSONObject) output.get(i);

                    ReceivedFile f;

                    try {
                        f = new ReceivedFile(o.getString("path"), Long.parseLong(o.getString("received")));
                        if (o.has("destination")) {
                            destinationList.put(o.getString("filename"), o.getString("destination"));
                        }
                    } catch (Exception e) {
                        continue;
                    }
                    this.receivedList.put(o.getString("filename"), f);
                }
            } catch (Exception e) {
                return;
            }
        }
    }

    public Tuple<String, String>[] getOwnFiles() {
        List<Tuple<String, String>> output = new ArrayList<Tuple<String, String>>();

        for (Map.Entry<String, Tuple<File, String>> entry : this.fileList.entrySet()) {
            output.add(new Tuple<String, String>(entry.getKey(), entry.getValue().b));
        }

        return output.toArray(new Tuple[0]);
    }

    public Map<String, String>[] getReceivedFiles() {
        ArrayList<Map<String, String>> list = new ArrayList<Map<String, String>>();

        for (Map.Entry<String, ReceivedFile> entry : this.receivedList.entrySet()) {
            Map<String, String> val = new HashMap<String, String>();
            val.put("filename", entry.getKey());
            val.put("progress", String.valueOf(entry.getValue().getProgress()));
            String destination = destinationList.get(entry.getKey());
            if (destination != null) val.put("destination", destination);

            list.add(val);
        }

        return list.toArray(new HashMap[0]);
    }

    public File getFile(String key) {
        Tuple<File, String> val = this.fileList.get(key);
        if (val != null) return val.a;
        return null;
    }

    public ReceivedFile getReceivedFile(String key) {
        return this.receivedList.get(key);
    }

    public long getFileSize(String fname) {
        File file = this.getFile(fname);
        if (file == null) {
            ReceivedFile tmp = this.getReceivedFile(fname);
            if (tmp == null) return 0;

            file = tmp.getFile();
        }
        return file.length();
    }

    public byte[] getChunk(String fname, int chunk) throws IOException {
        File file = this.getFile(fname);
        if (file == null) {
            ReceivedFile tmp = this.getReceivedFile(fname);
            if (tmp != null) {
                long received = tmp.getFileInfo().b;
                if ((received & (int) Math.pow(2, chunk)) > 0) file = tmp.getFile();
            }
        }

        if (file == null) throw new FileNotFoundException("Chunk not found");

        FileChannel channel = new FileInputStream(file).getChannel();
        channel.position(chunk * ReceivedFile.CHUNK_SIZE);
        ByteBuffer buf = ByteBuffer.allocate(ReceivedFile.CHUNK_SIZE);
        channel.read(buf);
        return buf.array();
    }

    public boolean addFile(String fname, InputStream input, String originalName) {
        //File output = new File(FILE_DIR, fname);
        File output = new File(context.getFilesDir(), fname);
        System.out.println(output.getAbsolutePath());
        try {
            output.createNewFile();
            FileOutputStream outStream = new FileOutputStream(output);

            byte[] buffer = new byte[1024];
            int bytesRead;
            while ((bytesRead = input.read(buffer)) != -1)
            {
                outStream.write(buffer, 0, bytesRead);
            }
            outStream.close();
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
        this.fileList.put(fname, new Tuple<File, String>(output, originalName));
        saveFileList();
        return true;
    }

    public void receiveNewFile(String fname, int size) {
        try {
            ReceivedFile f = new ReceivedFile(fname, 0);
            f.init(size);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    private void saveFileList() {
        JSONArray output = new JSONArray();
        for (Map.Entry<String, Tuple<File, String>> entry : this.fileList.entrySet()) {
            JSONObject obj = new JSONObject();
            try {
                obj.put("filename", entry.getKey());
                obj.put("path", entry.getValue().a.getPath());
                obj.put("original-name", entry.getValue().b);
                String destination = destinationList.get(entry.getKey());
                if (destination != null) obj.put("destination", destination);
            } catch (JSONException e) {
                continue;
            }
            output.put(obj);
        }

        try {
            FileWriter fw = new FileWriter(this.ownOutput);
            fw.write(output.toString());
            fw.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void saveReceivedList() {
        JSONArray output = new JSONArray();
        for (Map.Entry<String, ReceivedFile> entry : this.receivedList.entrySet()) {
            JSONObject obj = new JSONObject();
            try {
                Tuple<String, Long> info = entry.getValue().getFileInfo();
                obj.put("filename", entry.getKey());
                obj.put("path", info.a);
                obj.put("received", info.b);
                String destination = destinationList.get(entry.getKey());
                if (destination != null) obj.put("destination", destination);
            } catch (JSONException e) {
                continue;
            }
            output.put(obj);
        }

        try {
            FileWriter fw = new FileWriter(this.receivedOutput);
            fw.write(output.toString());
            fw.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public ArrayAdapter<Tuple<String, String>> getOwnFileAdapter(Activity activity, int itemTemplate) {
        return new OwnFileListAdapter(context, activity, itemTemplate, this);
    }
    public ArrayAdapter<Map<String, String>> getReceivedFileAdapter(Activity activity, int itemTemplate) {
        return new ReceivedFileListAdapter(activity, itemTemplate, this);
    }

    public String getDestination(String fname) {
        return this.destinationList.get(fname);
    }
}

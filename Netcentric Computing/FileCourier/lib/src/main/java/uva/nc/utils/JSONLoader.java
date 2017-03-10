package uva.nc.utils;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

/**
 * Created by bumbadadabum on 16-1-17.
 */

public final class JSONLoader {
    public static JSONObject loadJsonObject(File file) throws IOException, JSONException {
        FileInputStream inputStream = new FileInputStream(file);
        StringBuilder builder = new StringBuilder();

        int ch;
        while((ch = inputStream.read()) != -1){
            builder.append((char)ch);
        }

        String content = builder.toString();

        return new JSONObject(content);
    }

    public static JSONArray loadJsonArray(File file) throws IOException, JSONException {
        FileInputStream inputStream = new FileInputStream(file);
        StringBuilder builder = new StringBuilder();

        int ch;
        while((ch = inputStream.read()) != -1){
            builder.append((char)ch);
        }

        String content = builder.toString();

        return new JSONArray(content);
    }
}

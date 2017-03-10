package uva.nc.files;

import android.app.Activity;
import android.database.Cursor;
import android.net.Uri;
import android.provider.OpenableColumns;

/**
 * Created by bumbadadabum on 19-1-17.
 */

public final class FileUtils {
    public static String extractFileExtension(Activity act, Uri uri) {
        return getExtension(getFileName(act, uri));
    }

    public static String getExtension(String fname) {
        String[] parts = fname.split("\\.");
        if (parts.length < 2) return "";

        return "." + parts[parts.length - 1];
    }

    public static String getFileName(Activity act, Uri uri) {
        Cursor cursor = act.getContentResolver().query(uri, null, null, null, null);

        int nameIndex = cursor.getColumnIndex(OpenableColumns.DISPLAY_NAME);
        cursor.moveToFirst();

        return cursor.getString(nameIndex);
    }
}

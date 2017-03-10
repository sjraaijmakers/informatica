package uva.nc;

import android.app.Service;
import android.os.Binder;

public class LocalServiceBinder<S extends Service> extends Binder {

    private final S service;

    public LocalServiceBinder(S service) {
        this.service = service;
    }

    public S getService() {
        return service;
    }
}

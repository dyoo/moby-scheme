package com.phonegap.demo;


import android.content.Context;
import android.telephony.TelephonyManager;
import android.telephony.NeighboringCellInfo;

import java.util.List;
import java.util.ArrayList;

public class Telephony {
    private Context ctx;

    public Telephony(Context ctx) {
	this.ctx = ctx;
    }

    public List<CellInfo> getSignalStrengths() {
	System.out.println("Trying to get signal strengths!");
	TelephonyManager mgr = (TelephonyManager)
	    ctx.getSystemService(Context.TELEPHONY_SERVICE);
	List<NeighboringCellInfo> infos =
	    mgr.getNeighboringCellInfo();
	System.out.println("I see infos: " + infos);
	List<CellInfo> result = new ArrayList();
	for(NeighboringCellInfo info : infos) {
	    System.out.println(info.getCid() + "> " + info.getRssi());
	    result.add(new CellInfo(info.getCid(),
				    info.getRssi()));
	}
	return result;
    }
}
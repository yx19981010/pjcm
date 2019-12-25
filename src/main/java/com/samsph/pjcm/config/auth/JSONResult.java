package com.samsph.pjcm.config.auth;

import cn.hutool.json.JSONObject;

import java.util.Date;

public class JSONResult {
    public static String fillResultString(boolean isok, Integer code, String message, Object data, Date timeStamp){
        JSONObject jsonObject = new JSONObject(){{
            put("isok", isok);
            put("code", code);
            put("message", message);
            put("data", data);
            put("timeStamp",timeStamp);
        }};
        return jsonObject.toString();
    }
}

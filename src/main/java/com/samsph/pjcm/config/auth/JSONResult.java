package com.samsph.pjcm.config.auth;

import cn.hutool.json.JSONObject;

public class JSONResult {
    public static String fillResultString(boolean isok,Integer code, String message, Object data){
        JSONObject jsonObject = new JSONObject(){{
            put("isok", isok);
            put("code", code);
            put("message", message);
            put("data", data);
        }};
        return jsonObject.toString();
    }
}

package com.samsph.pjcm.config.constant;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * @author hujiahao
 */

public enum FundLevel {
    // 无
    NO(0),

    // 国家级
    NATIONAL(1),

    // 省部级
    PROVINCIAL(2),

    // 市厅级
    MUNICIPAL(3),

    // 院级
    HOSPITAL(4);

    private final int code;

    @JsonValue
    public int getCode() {
        return code;
    }

    @JsonCreator
    public static FundLevel getItem(int code){
        for(FundLevel item : values()){
            if(item.getCode() == code){
                return item;
            }
        }
        return null;
    }

    FundLevel(int code) {
        this.code = code;
    }

    @Override
    public String toString() {
        return code + "";
    }
}

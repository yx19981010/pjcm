package com.samsph.pjcm.config.constant;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

/**
 * @author hujiahao
 */

public enum Genre {
    // 论著：必填中英文摘要、关键字
    WORKS(1),

    // 综述：只需中文摘要和关键字
    OVERVIEW(2),

    // 论文：只需中文摘要和关键字
    PAPER(3),

    // 个案，无需中英文摘要
    CASE(4);


    private final int code;

    @JsonValue
    public int getCode() {
        return code;
    }

    @JsonCreator
    public static Genre getItem(int code){
        for(Genre item : values()){
            if(item.getCode() == code){
                return item;
            }
        }
        return null;
    }

    Genre(int code) {
        this.code = code;
    }

    @Override
    public String toString() {
        return code + "";
    }
}
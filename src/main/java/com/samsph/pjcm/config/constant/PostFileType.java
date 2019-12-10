package com.samsph.pjcm.config.constant;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * @author hujiahao
 */

public enum PostFileType {
    /**
     * 稿件
     */
    POST(1),

    /**
     * 伦理委员会批文
     */
    ETHICS(2),

    /**
     * 推荐信
     */
    LETTER(3),

    /**
     * 基金批文
     */
    FUND(4),

    /**
     * 缴费证明
     */
    PAYMENT(5);

    private final int code;

    PostFileType(int code) {
        this.code = code;
    }

    @JsonValue
    public int getCode() {
        return code;
    }

    @JsonCreator
    public static PostFileType getItem(int code) {
        for (PostFileType item : values()) {
            if (item.getCode() == code) {
                return item;
            }
        }
        return null;
    }
}
package com.samsph.pjcm.config.constant;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * 自定义布尔类
 *
 * @author hujiahao
 */

public enum MyBoolean {
    /**
     * 假
     */

    FALSE(0),

    /**
     * 真
     */
    TRUE(1),

    /**
     * 缺省
     */
    DEFAULT(2);

    private final int code;

    public int getCode() {
        return code;
    }

    public static MyBoolean getItem(int code) {
        for (MyBoolean item : values()) {
            if (item.getCode() == code) {
                return item;
            }
        }
        return null;
    }

    public static MyBoolean getItem(Boolean a) {
        if (a == null) {
            return DEFAULT;
        } else if (a) {
            return TRUE;
        } else {
            return FALSE;
        }
    }

    MyBoolean(int code) {
        this.code = code;
    }

    @Override
    public String toString() {
        return code + "";
    }
}

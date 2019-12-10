package com.samsph.pjcm.config.constant;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * 投稿领域
 *
 * @author hujiahao
 */

public enum Field {

    // 基础医学
    BASIC_MEDICINE(1),

    // 临床医学
    CLINICAL_MEDICINE(2),

    // 内科医学
    INTERNAL_MEDICINE(3),

    // 基础中医
    BASIC_CHINESE_MEDICINE(4),

    // 中医针灸
    CHINESE_ACUPUNCTURE(5),

    // 外科医学
    SURGICAL_MEDICINE(6),

    // 神经医学
    NEUROLOGY(7),

    // 预防医学
    PREVENTIVE_MEDICINE(8),

    // 皮肤医学
    DERMATOLOGY(9),

    // 检验医学
    LABORATORY_MEDICINE(10),

    // 保健医学
    HEALTH_CARE_MEDICINE(11),

    // 康复医学
    REHABILITATION_MEDICINE(12),

    // 护理学
    NURSING(13);

    private final int code;

    public static final int TOTAL_FIELD = 13;

    @JsonCreator
    public static Field getItem(int code){
        for(Field item : values()){
            if(item.getCode() == code){
                return item;
            }
        }
        return null;
    }

    @JsonValue
    public int getCode() {
        return code;
    }


    Field(int code) {
        this.code = code;
    }

    @Override
    public String toString() {
        return code + "";
    }
}

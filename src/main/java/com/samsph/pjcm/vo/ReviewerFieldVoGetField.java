package com.samsph.pjcm.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ReviewerFieldVoGetField {
    /**
     * 标识
     */
    private int id;

    /**
     * 领域
     */
    private int field;
}

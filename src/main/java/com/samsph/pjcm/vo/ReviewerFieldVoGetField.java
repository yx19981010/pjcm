package com.samsph.pjcm.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ReviewerFieldVoGetField {
    /**
     * 审稿人领域标识
     */
    private Integer id;

    /**
     * 审稿人id
     */
    private Integer field;
}

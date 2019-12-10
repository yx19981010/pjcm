package com.samsph.pjcm.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class EditorFieldVoGetField {
    /**
     * 标识
     */
    private Integer id;

    /**
     * 领域
     */
    private Integer field;
}

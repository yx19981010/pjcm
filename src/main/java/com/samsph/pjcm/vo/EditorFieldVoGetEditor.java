package com.samsph.pjcm.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class EditorFieldVoGetEditor {
    /**
     * 标识
     */
    private Integer id;

    /**
     * 编辑id
     */
    private Integer editorUid;
}

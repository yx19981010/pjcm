package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ApiModel(description = "某个领域下的编辑领域返回模型")
public class EditorFieldVoGetEditor {
    /**
     * 标识
     */
    @ApiModelProperty("编辑领域标识")
    private Integer id;

    /**
     * 编辑id
     */
    @ApiModelProperty("编辑id")
    private Integer editorUid;

    /**
     * 用户名
     */
    @ApiModelProperty("用户名")
    private String userName;

    /**
     * 电子邮件
     */
    @ApiModelProperty("电子邮件")
    private String email;
}

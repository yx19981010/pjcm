package com.samsph.pjcm.vo;

import com.samsph.pjcm.config.constant.Field;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;


@ApiModel(description = "编辑-领域添加模型")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class EditorFieldVoPost {
    /**
     * 编辑的用户id
     */
    @ApiModelProperty("编辑的用户id")
    @NotNull(message = "编辑id不能为空")
    @Min(value = 1,message = "编辑id必须为正整数")
    private Integer editorUid;

    /**
     * 领域
     */
    @ApiModelProperty("领域id")
    @NotNull(message = "领域id为空或领域id不在范围内")
    private Field field;
}

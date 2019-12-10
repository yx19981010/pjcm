package com.samsph.pjcm.vo;

import com.samsph.pjcm.config.constant.Field;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

@ApiModel(description = "编辑-领域修改模型")
@Data
public class EditorFieldVoPut {
    /**
     * 标识
     */
    @ApiModelProperty("编辑-领域标识")
    @NotNull(message = "id不能为空")
    @Min(value = 1,message = "id必须为正整数")
    private Integer id;

    /**
     * 编辑的用户id
     */
    @ApiModelProperty("编辑的用户id")
    @Min(value = 1,message = "编辑id必须为正整数")
    private Integer editorUid;

    /**
     * 领域
     */
    @ApiModelProperty("领域")
    @Min(value = 1,message = "领域id不低于1")
    @Max(value = Field.TOTAL_FIELD,message = "领域id超过上限")
    private Integer field;
}

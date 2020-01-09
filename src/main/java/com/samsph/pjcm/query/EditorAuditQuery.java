package com.samsph.pjcm.query;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

/**
 * @author hujahao
 */
@Data
@ApiModel(description = "编辑审核请求实体（初审/退回修改/发表前&格式审核/审缴费证明）")
public class EditorAuditQuery {
    @ApiModelProperty("标识号")
    @NotNull(message = "id不能为空")
    private Integer id;

    @ApiModelProperty("是否 通过初审/退回修改/返给审稿人/通过缴费证明审核")
    @NotNull(message = "pass不能为空")
    private Boolean pass;

    @ApiModelProperty("意见，pass为假时必填该字段")
    @Size(max = 50, message = "意见不得超过50字符")
    private String comment;
}

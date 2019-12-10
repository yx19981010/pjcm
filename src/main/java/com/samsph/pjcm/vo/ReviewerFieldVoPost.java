package com.samsph.pjcm.vo;

import com.samsph.pjcm.config.constant.Field;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ApiModel(description = "审稿人-领域添加模型")
public class ReviewerFieldVoPost {
    /**
     * 审稿人id
     */
    @ApiModelProperty("审稿人的用户id")
    @NotNull(message = "审稿人id不能为空")
    @Min(value = 1,message = "审稿人id必须为正整数")
    private Integer reviewerUid;

    /**
     * 领域
     */
    @ApiModelProperty("领域id")
    @NotNull(message = "领域id为空或领域id不在范围内")
    private Field field;
}

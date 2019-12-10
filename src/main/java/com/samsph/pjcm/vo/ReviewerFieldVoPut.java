package com.samsph.pjcm.vo;

import com.samsph.pjcm.config.constant.FieldTotal;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ReviewerFieldVoPut {
    /**
     * 标识
     */
    @NotNull(message = "id不能为空")
    @Min(value = 1,message = "id必须为正整数")
    private Integer id;

    /**
     * 审稿人id
     */
    @Min(value = 1,message = "审稿人id必须为正整数")
    private Integer reviewerUid;

    /**
     * 领域
     */
    @Min(value = 1,message = "领域id不低于1")
    @Max(value = FieldTotal.TOTAL_FIELD,message = "领域id超过上限")
    private Integer field;
}

package com.samsph.pjcm.query;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.Digits;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

/**
 * @author hujiahao
 */
@ApiModel(description = "更新版面费时请求实体")
@Data
public class PostLayOutFeeQuery {
    @ApiModelProperty("标识号")
    @NotNull(message = "id不能为空")
    private Integer id;

    @ApiModelProperty("版面费。小数不超过两位。")
    @NotNull(message = "fee不能为空")
    @Pattern(regexp = "^(([1-9]\\d*)|([0]))(\\.(\\d){0,2})?$",message ="fee小数不能超过两位" )
    private Double fee;
}

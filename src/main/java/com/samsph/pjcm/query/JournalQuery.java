package com.samsph.pjcm.query;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;

/**
 * @author hujiahao
 */
@Data
@AllArgsConstructor
@ApiModel(description = "期刊controller的请求实体")
public class JournalQuery {
    @ApiModelProperty("标识号")
    @NotNull(message = "id不能为空", groups = {Update.class})
    @Null(message = "id必须为空", groups = {Add.class})
    private Integer id;

    @ApiModelProperty("年号")
    @NotNull(message = "年号不能为空", groups = {Add.class})
    @Max(value = 2050, message = "年号至大为2050", groups = {Add.class, Update.class})
    @Min(value = 2000, message = "年号至少为2000", groups = {Add.class, Update.class})
    private Integer year;

    @ApiModelProperty("月号")
    @NotNull(message = "月号不能为空", groups = {Add.class})
    @Max(value = 12, message = "月号至大为12", groups = {Add.class, Update.class})
    @Min(value = 1, message = "月号至少为1", groups = {Add.class, Update.class})
    private Integer month;

    @ApiModelProperty("卷号")
    @NotNull(message = "卷号不能为空", groups = {Add.class})
    @Min(value = 1, message = "卷号至少为1", groups = {Add.class, Update.class})
    private Integer volume;

    @ApiModelProperty("期号")
    @NotNull(message = "期号不能为空", groups = {Add.class})
    @Min(value = 1, message = "期号至少为1", groups = {Add.class, Update.class})
    private Integer number;
}

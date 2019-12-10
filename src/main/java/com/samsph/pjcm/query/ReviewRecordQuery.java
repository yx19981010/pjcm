package com.samsph.pjcm.query;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;

/**
 * @author hujiahao
 */
@Data
@ApiModel(description = "审稿记录请求实体")
public class ReviewRecordQuery {
    @ApiModelProperty("稿件id")
    @NotNull(message = "pid不能为空")
    private Integer pid;

    @ApiModelProperty("是否否决")
    @NotNull(groups = {Add.class}, message = "reject不能为空")
    @Null(groups = {Add2.class}, message = "reject必须为空")
    private Boolean reject;

    @ApiModelProperty("否决意见")
    @Null(groups = {Add2.class}, message = "rejectComment必须为空")
    private String rejectComment;

    @ApiModelProperty("是否转送")
    @NotNull(groups = {Add.class}, message = "toForward不能为空")
    @Null(groups = {Add2.class}, message = "toForward必须为空")
    private Boolean toForward;

    @ApiModelProperty("转送意见")
    @Null(groups = {Add2.class}, message = "forwardComment必须为空")
    private String forwardComment;

    @ApiModelProperty("是否要修改")
    @NotNull(message = "toRevise不能为空")
    private Boolean toRevise;

    @ApiModelProperty("修改意见")
    private String reviseComment;
}

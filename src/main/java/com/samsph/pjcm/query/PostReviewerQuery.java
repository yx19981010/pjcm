package com.samsph.pjcm.query;

import com.samsph.pjcm.config.constant.MyBoolean;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;

/**
 * @author hujiahao
 */


@Data
@AllArgsConstructor
@ApiModel(description = "稿件-审稿人请求实体")
public class PostReviewerQuery {
    @ApiModelProperty("稿件id")
    @NotNull(message = "pid不能为空", groups = {Add.class, Update.class})
    private Integer pid;

    @ApiModelProperty("审稿人id")
    @NotNull(groups = {Add.class}, message = "uid不能为空")
    @Null(groups = {Update.class}, message = "uid必须为空")
    private Integer reviewerUid;

    @ApiModelProperty("是否接受该审稿邀约")
    @Null(groups = {Add.class}, message = "accept必须为空")
    @NotNull(groups = {Update.class}, message = "accept不能为空")
    private Boolean accept;
}

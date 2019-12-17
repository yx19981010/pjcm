package com.samsph.pjcm.query;

import com.samsph.pjcm.config.constant.*;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;

/**
 * @author hujiahao
 */
@Data
@ApiModel(description = "审稿记录请求实体")
public class ReviewRecordQuery {
    @ApiModelProperty("稿件id")
    @NotNull(groups = {Add.class,Add2.class},message = "pid不能为空")
    private Integer pid;

    @ApiModelProperty("是否否决（退稿）")
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

    @ApiModelProperty("是否修改（修改后再审）")
    @NotNull(groups = {Add.class,Add2.class},message = "toRevise不能为空")
    private Boolean toRevise;

    @ApiModelProperty("修改意见")
    private String reviseComment;

    @ApiModelProperty("给编辑部的意见")
    private String adviceToNewsroom;

    @ApiModelProperty("政治方面")
    @NotNull(groups = {Add.class,Add2.class},message = "political不能为空")
    @Min(groups = {Add.class,Add2.class},value = PoliticalType.MIN,message = "political不低于"+PoliticalType.MIN)
    @Max(groups = {Add.class,Add2.class},value = PoliticalType.MAX,message = "political不超过"+PoliticalType.MAX)
    private Integer political;

    @ApiModelProperty("学术方面")
    @NotNull(groups = {Add.class,Add2.class},message = "academic不能为空")
    @Min(groups = {Add.class,Add2.class},value = AcademicType.MIN,message = "academic不低于"+AcademicType.MIN)
    @Max(groups = {Add.class,Add2.class},value = AcademicType.MAX,message = "academic不超过"+AcademicType.MAX)
    private Integer academic;

    @ApiModelProperty("科学性方面")
    @NotNull(groups = {Add.class,Add2.class},message = "scientific不能为空")
    @Min(groups = {Add.class,Add2.class},value = ScientificType.MIN,message = "scientific不低于"+ScientificType.MIN)
    @Max(groups = {Add.class,Add2.class},value = ScientificType.MAX,message = "scientific不超过"+ScientificType.MAX)
    private Integer scientific;

    @ApiModelProperty("文字方面")
    @NotNull(groups = {Add.class,Add2.class},message = "text不能为空")
    @Min(groups = {Add.class,Add2.class},value = TextType.MIN,message = "text不低于"+TextType.MIN)
    @Max(groups = {Add.class,Add2.class},value = TextType.MAX,message = "text不超过"+TextType.MAX)
    private Integer text;

    @ApiModelProperty("实用性方面")
    @NotNull(groups = {Add.class,Add2.class},message = "practicality不能为空")
    @Min(groups = {Add.class,Add2.class},value = PracticalityType.MIN,message = "practicality不低于"+PracticalityType.MIN)
    @Max(groups = {Add.class,Add2.class},value = PracticalityType.MAX,message = "practicality不超过"+PracticalityType.MAX)
    private Integer practicality;

    @ApiModelProperty("总评价")
    @NotNull(groups = {Add.class,Add2.class},message = "evaluation不能为空")
    @Min(groups = {Add.class,Add2.class},value = EvaluationType.MIN,message = "evaluation不低于"+ EvaluationType.MIN)
    @Max(groups = {Add.class,Add2.class},value = EvaluationType.MAX,message = "evaluation不超过"+ EvaluationType.MAX)
    private Integer evaluation;

    @ApiModelProperty("在否决和需要修改的情况下可以为空，刊用类型(包括了１．全文发表  ２．摘要发表  ３．综合发表  4.退修后发表 4种类型)")
    @Min(groups = {Add.class,Add2.class},value = PublishType.MIN,message = "publish不低于"+ PublishType.MIN)
    @Max(groups = {Add.class,Add2.class},value = PublishType.MAX,message = "publish不超过"+ PublishType.MAX)
    private Integer publish;

}

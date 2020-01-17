package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import java.util.Date;

/**
 * 审稿记录视图对象
 *
 * @author hujahao
 */
@AllArgsConstructor
@NoArgsConstructor
@Data
@ApiModel(description = "审稿记录返回模型")
public class ReviewRecordVO {
    /**
     * 审稿记录id
     */
    @ApiModelProperty("审稿记录id")
    private Integer id;

    /**
     * 稿件id
     */
    @ApiModelProperty("稿件id")
    private Integer pid;

    /**
     * 审稿人id
     */
    @ApiModelProperty("审稿人id")
    private Integer uid;

    /**
     * 审稿人姓名
     */
    @ApiModelProperty("审稿人姓名")
    private String review_name;

    /**
     * 属于第几轮审阅
     */
    @ApiModelProperty("属于第几轮审阅")
    private Integer count;

    /**
     * 记录创建时间
     */
    @ApiModelProperty("记录创建时间")
    private Date createTime;

    /**
     * 是否否决
     */
    @ApiModelProperty("是否否决")
    private Integer reject;

    /**
     * 否决意见
     */
    @ApiModelProperty("否决意见")
    private String rejectComment;

    /**
     * 是否要转送
     */
    @ApiModelProperty("是否要转送")
    private Integer toForward;

    /**
     * 转送意见
     */
    @ApiModelProperty("转送意见")
    private String forwardComment;

    /**
     * 是否要修改
     */
    @ApiModelProperty("是否要修改")
    private Integer toRevise;

    /**
     * 修改意见
     */
    @ApiModelProperty("修改意见")
    private String reviseComment;

    /**
     * 政治方面
     */
    @ApiModelProperty("政治方面")
    private Integer political;

    /**
     * 学术方面
     */
    @ApiModelProperty("学术方面")
    private Integer academic;

    /**
     * 科学性方面
     */
    @ApiModelProperty("科学性方面")
    private Integer scientific;

    /**
     * 文字方面
     */
    @ApiModelProperty("文字方面")
    private Integer text;

    /**
     * 实用性方面
     */
    @ApiModelProperty("实用性方面")
    private Integer practicality;

    /**
     * 总评价
     */
    @ApiModelProperty("总评价")
    private Integer evaluation;

    /**
     * 刊用类型
     */
    @ApiModelProperty("刊用类型")
    private Integer publish;
}

package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

/**
 * 审稿记录视图对象
 *
 * @author hujahao
 */
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ReviewRecordVO {
    /**
     * 审稿记录id
     */
    private Integer id;

    /**
     * 稿件id
     */
    private Integer pid;

    /**
     * 审稿人id
     */
    private Integer uid;

    /**
     * 属于第几轮审阅
     */
    private Integer count;

    /**
     * 记录创建时间
     */
    private Date createTime;

    /**
     * 是否否决
     */
    private Integer reject;

    /**
     * 否决意见
     */
    private String rejectComment;

    /**
     * 是否要转送
     */
    private Integer toForward;

    /**
     * 转送意见
     */
    private String forwardComment;

    /**
     * 是否要修改
     */
    private Integer toRevise;

    /**
     * 修改意见
     */
    private String reviseComment;

    /**
     * 政治方面
     */
    private Integer political;

    /**
     * 学术方面
     */
    private Integer academic;

    /**
     * 科学性方面
     */
    private Integer scientific;

    /**
     * 文字方面
     */
    private Integer text;

    /**
     * 实用性方面
     */
    private Integer practicality;

    /**
     * 总评价
     */
    private Integer evaluation;
}

package com.samsph.pjcm.config.constant;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

/**
 * @author hujiahao
 */

public enum PostStatus {
    // 已投稿
    TO_BE_SUBMITTED(0),

    // 待初审
    PENDING_FIRST_EXAM(1),

    // 初审否决
    FIRST_EXAM_REJECTED(2),

    // 待选择审稿人
    REVIEWER_TO_BE_SELECTED(3),

    // 首次审稿
    FIRST_REVIEW(4),

    // 审稿人否决
    REVIEWER_REJECT(5),

    // 待退回稿件
    TO_BE_RETURNED(6),

    // 编辑否决
    EDITOR_REJECT(7),

    // 稿件待修改
    TO_BE_REVISED(8),

    // 再次审稿
    RE_REVIEW(9),

    // 格式待审核
    FORMAT_TO_BE_REVIEWED(10),

    // 格式待修改
    FORMAT_TO_BE_MODIFIED(11),

    // 待确定版面费
    LAYOUT_FEE_TO_BE_DETERMINED(12),

    // 缴费证明待上传
    CERTIFICATE_TO_BE_UPLOADED(13),

    // 缴费证明待审核
    PAYMENT_TO_BE_EXAMINED(14),

    // 投稿成功
    SUCCESS(15);

    private final int code;

    @JsonValue
    public int getCode() {
        return code;
    }

    @JsonCreator
    public static PostStatus getItem(int code){
        for(PostStatus item : values()){
            if(item.getCode() == code){
                return item;
            }
        }
        return null;
    }

    PostStatus(int code) {
        this.code = code;
    }

    @Override
    public String toString() {
        return code + "";
    }
}

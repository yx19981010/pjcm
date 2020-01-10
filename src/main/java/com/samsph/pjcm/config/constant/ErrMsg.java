package com.samsph.pjcm.config.constant;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

/**
 * 错误消息枚举类
 *
 * @author hujiahao
 */

public enum ErrMsg {
    NOT_CONTRIBUTOR(1009, "不是投稿人"),

    NOT_EDITOR(1010, "不是编辑"),

    NOT_REVIEWER(1010, "不是审稿人"),

    NOT_ADMIN(1012, "不是管理员"),

    CANNOT_REVIEW(1000, "不可进行审稿"),

    JOURNAL_EXISTS(1001, "期刊已存在"),

    REVIEW_RECORD_EXISTS(1001, "审稿记录已存在"),

    POST_REVIEWER_EXISTS(1005, "已为稿件选择该审稿人"),

    JOURNAL_NOT_FOUND(1002, "期刊未找到"),

    POST_NOT_FOUND(1004, "稿件未找到"),

    POST_REVIEWER_NOT_FOUND(1006, "稿件-审稿人记录未找到"),

    REVIEWER_HAS_REPLIED(1010, "审稿人已答复审稿"),

    UNSUPPORTED_POST_FILE_TYPE(1008, "不支持的稿件相关文件类型"),

    POST_FILE_NOT_EXISTS(2000, "文件不存在"),

    POST_FILE_READ_ERROR(2000, "文件读取异常"),

    NOT_FUND_PROJECT(1013, "不是基金项目"),

    WRONG_STATUS(1014, "错误的稿件状态"),

    CANNOT_REVISE_OR_FORWARD(1000, "不能转送或修改"),

    CANNOT_REVISE_OR_REJECT(1000, "不能否决或修改"),

    PUBLISH_NEEDED(1000, "刊用需要选择类型"),

    REVISE_COMMENT_NEEDED(1000, "建议修改需填写意见"),

    FORWARD_COMMENT_NEEDED(1000, "转送需填写意见"),

    REJECT_COMMENT_NEEDED(1000, "不通过需填写意见"),

    TITLE_NEEDED(1000, "缺少标题信息"),

    FIELD_NEEDED(1000, "缺少投稿领域信息"),

    GENRE_NEEDED(1000, "缺少体裁信息"),

    WRITERS_INFO_NEEDED(1000, "缺少作者信息"),

    CORRESPONDENCE_AUTHOR_NEEDED(1000, "缺少通讯作者信息"),

    FUND_LEVEL_NEEDED(1000, "缺少基金等级信息"),

    REFERENCES_NEEDED(1000, "缺少参考文献信息"),

    INCOMPLETE_POST_FILES(1000, "文件上传不全"),

    INCOMPLETE_INFO_REQUIRED_FOR_THE_GENRE(1000, "该体裁所需信息不完整"),

    UNSUPPORTED_GENRE(1000, "不支持的体裁"),

    UNSUPPORTED_FUND_LEVEL(1000, "不支持的基金级别"),

    PAYMENT_FILE_NOT_UPLOADED(1000, "缴费证明或授权转让书未上传"),

    INVOICE_REQUIRED_OR_NOT_NEEDED(1000, "缺少是否需要发票"),

    INCOMPLETE_INVOICE_INFO(2000, "发票信息不完整"),

    WRONG_TIME(1000, "错误的起止时间"),

    UNSUPPORTED_STATUS(1000, "不支持的稿件状态"),

    CONTRIBUTOR_NOT_FOUND(1000, "投稿人未找到"),

    REVIEWER_NOT_FOUND(1000, "审稿人未找到"),

    STATUSES_NEEDED(1000, "缺少稿件状态信息"),

    UNSUPPORTED_FIELD(1000, "不支持的投稿领域"), INCOMPLETE_FIRST_AUTHOR_INFO(1000, "不完整的一作信息");

    @Getter
    private final int code;
    private final String msg;

    @JsonValue
    public String getMsg() {
        return msg;
    }

    ErrMsg(int code, String msg) {
        this.msg = msg;
        this.code = code;
    }
}

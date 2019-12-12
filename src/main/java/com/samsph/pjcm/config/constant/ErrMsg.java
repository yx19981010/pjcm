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

    JOURNAL_NOT_FOUND(1002, "期刊未找到"),

    POST_NOT_FOUND(1004, "投稿未找到"),

    POST_REVIEWER_NOT_FOUND(1006, "稿件-审稿人记录未找到"),

    POST_REVIEWER_EXISTS(1005, "稿件已选择该审稿人"),

    REVIEWER_HAS_REPLIED(1010, "审稿人已答复审稿请求"),

    POST_FILE_CANNOT_UPLOAD(1003, "无法上传稿件"),

    LETTER_FILE_CANNOT_UPLOAD(1004, "无法上传推荐信"),

    ETHICS_FILE_CANNOT_UPLOAD(1005, "无法上传伦理委员会批文"),

    FUND_FILE_CANNOT_UPLOAD(1006, "无法上传基金批文"),

    PAYMENT_FILE_CANNOT_UPLOAD(1007, "无法上传缴费证明"),

    UNSUPPORTED_POST_FILE_TYPE(1008, "不支持的稿件相关文件类型"),

    POST_FILE_NOT_EXISTS(2000, "文件不存在"),

    POST_FILE_READ_ERROR(2000, "文件读取异常"),

    NO_FUND(1013, "不是基金项目"),

    WRONG_STATUS(1014, "错误的稿件状态"),

    CANNOT_REVISE_OR_FORWARD(1000, "不能转送或修改"),

    REVISE_COMMENT_NEEDED(1000, "建议修改需填写意见"),

    FORWARD_COMMENT_NEEDED(1000, "转送需填写意见"),

    REJECT_COMMENT_NEEDED(1000, "不通过需填写意见"),

    TITLE_NEEDED(1000, "缺少标题信息"),

    FIELD_NEEDED(1000, "缺少投稿领域信息"),

    GENRE_NEEDED(1000, "缺少体裁信息"),

    WRITERS_INFO_NEEDED(1000, "缺少作者信息"),

    FUND_LEVEL_NEEDED(1000, "缺少基金等级信息"),

    INCOMPLETE_POST_FILES(1000, "投稿相关文件上传不全"),

    INCOMPLETE_KEYWORDS_AND_ABSTRACTS(1000, "关键词或摘要不完整"),

    UNSUPPORTED_GENRE(1000, "不支持的体裁"),

    PAYMENT_FILE_NOT_UPLOADED(1000, "缴费证明未上传"),

    INVOICE_REQUIRED_OR_NOT_NEEDED(1000, "缺少是否需要发票"),

    INCOMPLETE_INVOICE_INFO(2000, "发票信息不完整");


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

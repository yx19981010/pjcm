package com.samsph.pjcm.query;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

import javax.persistence.Column;
import javax.validation.constraints.*;

import static com.samsph.pjcm.config.constant.Field.LEAST_FIELD;
import static com.samsph.pjcm.config.constant.Field.TOTAL_FIELD;

/**
 * @author hujiahao
 */
@Data
@ApiModel(description = "创建/更新投稿请求实体")
public class
PostQuery {
    // Add组：新建该post；
    // Update组：在提交初审前，用户可修改投稿基本信息；
    // Update2组：退回修改时，用户可修改部分投稿信息；
    // 中英文关键字、摘要的填写与否根据投稿题材而定，在controller层校验

    @ApiModelProperty("标识号")
    @Null(message = "id必须为空", groups = {Add.class})
    @NotNull(message = "id不能为空", groups = {Update.class, Update2.class})
    private Integer id;

    @ApiModelProperty("投稿领域")
    @Min(value = LEAST_FIELD, message = "投稿领域错误")
    @Max(value = TOTAL_FIELD, message = "投稿领域错误")
    @NotNull(message = "field不能为空", groups = {Add.class})
    @Null(message = "field必须为空", groups = {Update2.class})
    private Integer field;

    @ApiModelProperty("基金级别")
    @NotNull(message = "fundLevel不能为空", groups = {Add.class})
    @Null(message = "fundLevel必须为空", groups = {Update2.class})
    private Integer fundLevel;

    @ApiModelProperty("作者信息")
    @NotBlank(message = "writersInfo不能为空", groups = {Add.class})
    @Null(message = "writersInfo必须为空", groups = {Update2.class})
    private String writersInfo;

    @ApiModelProperty("通讯作者名")
    @NotBlank(message = "corAuName不能为空", groups = {Add.class})
    @Null(message = "corAuName必须为空", groups = {Update2.class})
    private String corAuName;

    @ApiModelProperty("一作姓名")
    @NotBlank(message = "fAuName不能为空", groups = {Add.class})
    @Null(message = "fAuName必须为空", groups = {Update2.class})
    private String fAuName;

    @ApiModelProperty("一作工作单位")
    @NotBlank(message = "fAuEmployer不能为空", groups = {Add.class})
    @Null(message = "fAuEmployer必须为空", groups = {Update2.class})
    private String fAuEmployer;

    @ApiModelProperty("一作邮箱")
    @NotBlank(message = "fAuEmail不能为空", groups = {Add.class})
    @Null(message = "fAuEmail必须为空", groups = {Update2.class})
    private String fAuEmail;

    @ApiModelProperty("一作手机号")
    @NotBlank(message = "fAuPhone不能为空", groups = {Add.class})
    @Null(message = "fAuPhone必须为空", groups = {Update2.class})
    private String fAuPhone;

    @ApiModelProperty("一作职称")
    @NotBlank(message = "fAuTitle不能为空", groups = {Add.class})
    @Null(message = "fAuTitle必须为空", groups = {Update2.class})
    private String fAuTitle;

    @ApiModelProperty("一作学历")
    @NotBlank(message = "fAuEducation不能为空", groups = {Add.class})
    @Null(message = "fAuEducation必须为空", groups = {Update2.class})
    private String fAuEducation;

    @ApiModelProperty("文章标题")
    @Length(max = 30, message = "标题不能超过30字")
    @NotBlank(message = "title不能为空", groups = {Add.class})
    private String title;

    @ApiModelProperty("投稿体裁")
    @NotNull(message = "genre不能为空", groups = {Add.class})
    private Integer genre;

    @ApiModelProperty("英文文章标题")
    @Length(max = 100, groups = {Add.class, Update.class, Update2.class}, message = "英文标题不能超过100字")
    private String titleEn;

    @ApiModelProperty("中文关键字")
    @Length(max = 240, groups = {Add.class, Update.class, Update2.class}, message = "abstractZh不能超过240字")
    private String keywordsZh;

    @ApiModelProperty("英文关键字")
    @Length(max = 240, groups = {Add.class, Update.class, Update2.class}, message = "abstractZh不能超过240字")
    private String keywordsEn;

    @ApiModelProperty("中文摘要")
    @Length(max = 300, groups = {Add.class, Update.class, Update2.class}, message = "abstractZh不能超过300字")
    private String abstractZh;

    @ApiModelProperty("英文摘要")
    @Length(max = 2400, groups = {Add.class, Update.class, Update2.class}, message = "abstractEn不能超过2400字")
    private String abstractEn;

    @ApiModelProperty("参考文献")
    private String referencesList;
}

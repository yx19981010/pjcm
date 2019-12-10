package com.samsph.pjcm.query;

import com.samsph.pjcm.config.constant.Field;
import com.samsph.pjcm.config.constant.FundLevel;
import com.samsph.pjcm.config.constant.Genre;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.*;
import java.util.List;

/**
 * @author hujiahao
 */
@Data
@ApiModel(description = "创建/更新投稿请求实体")
public class
PostQuery {
    // Add组：新建该post；
    //        不填id，必填field、title、genre、fundLevel、writersInfo
    // Update组：在提交初审前，用户可修改投稿基本信息；
    //           必填id，其它选填
    // Update2组：退回修改时，用户可修改部分投稿信息；
    //           必填id，不填field、genre、fundLevel、writersInfo，
    // 中英文关键字、摘要根据投稿题材而定，在controller层校验

    @ApiModelProperty("标识号")
    @Null(message = "id必须为空", groups = {Add.class})
    @NotNull(message = "id不能为空", groups = {Update.class, Update2.class})
    private Integer id;

    @ApiModelProperty("投稿领域")
    @NotNull(message = "field不能为空", groups = {Add.class})
    @Null(message = "field必须为空", groups = {Update2.class})
    private Field field;


    @ApiModelProperty("投稿体裁")
    @NotNull(message = "genre不能为空", groups = {Add.class})
    @Null(message = "genre必须为空", groups = {Update2.class})
    private Genre genre;

    @ApiModelProperty("基金级别")
    @NotNull(message = "fundLevel不能为空", groups = {Add.class})
    @Null(message = "fundLevel必须为空", groups = {Update2.class})
    private FundLevel fundLevel;

    // TODO：writersInfo须符合某一格式

    @ApiModelProperty("作者信息")
    @NotBlank(message = "writersInfo不能为空", groups = {Add.class})
    @Null(message = "writersInfo必须为空", groups = {Update2.class})
    private String writersInfo;

    @ApiModelProperty("文章标题")
    @Length(max = 30, message = "标题不能超过30字")
    @NotBlank(message = "title不能为空", groups = {Add.class})
    private String title;

    // TODO：中英文关键字须符合某一格式

    @ApiModelProperty("中文关键字")
    private String keywordsZh;

    @ApiModelProperty("英文关键字")
    private String keyWordsEn;

    @ApiModelProperty("中文摘要")
    @Length(max = 300, message = "abstractZh不能超过300字")
    private String abstractZh;

    @ApiModelProperty("英文摘要")
    @Length(max = 2500, message = "abstractEn不能超过2500字")
    private String abstractEn;
}

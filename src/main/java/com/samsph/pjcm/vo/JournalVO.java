package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 *
 * @author hujiahao
 */

@ApiModel(description="期刊详细信息实体类")
@Data
public class JournalVO extends JournalSimpleVO {
    @ApiModelProperty("期刊包含论文列表")
    private List<PostSimpleVO> posts;

    @ApiModelProperty("包含论文数")
    private Integer count;
}

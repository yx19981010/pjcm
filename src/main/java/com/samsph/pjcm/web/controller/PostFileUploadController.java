package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.constant.FileUploadPath;
import com.samsph.pjcm.config.constant.PostFileUploadType;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.utils.FileUtil;
import com.samsph.pjcm.service.PostService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import javax.validation.constraints.NotNull;

/**
 * 文件上传控制器
 *
 * @author hujiahao
 */
@Slf4j
@Validated
@RestController
@Api(tags = "5. 投稿相关文件上传")
@RequestMapping("/api/v1/postFilesUpload/{id}")
public class PostFileUploadController {
    @Resource(name = "postServiceImpl")
    PostService postService;

    @PutMapping()
    @ApiOperation(value = "投稿人上传稿件/伦理委员会批文/基金批文/缴费证明")
    public AjaxResponse upload(@NotNull @PathVariable Integer id,
                               @NotNull @RequestParam PostFileUploadType type,
                               @NotNull @RequestParam MultipartFile file) {
        // TODO: 以某种方式获得当前操作用户，检查其是否为该稿件的投稿人

        String pathname = FileUtil.FileUpload(FileUploadPath.PostFileUploadPath, file);

        switch (type) {
            case POST:

                break;
            case FUND:
                break;
            case ETHICS:
                break;
            case LETTER:
                break;
            case PAYMENT:
                break;
            default:
        }

        return AjaxResponse.success();
    }


}

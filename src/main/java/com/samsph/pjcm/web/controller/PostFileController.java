package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.constant.*;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.config.utils.FileUtil;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.model.PostReviewer;
import com.samsph.pjcm.service.PostReviewerService;
import com.samsph.pjcm.service.PostService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.tomcat.util.http.fileupload.IOUtils;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.constraints.NotNull;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Date;

import static com.samsph.pjcm.config.DevUserId.*;
import static com.samsph.pjcm.config.constant.ErrMsg.POST_FILE_NOT_EXISTS;
import static com.samsph.pjcm.config.constant.ErrMsg.POST_FILE_READ_ERROR;

/**
 * @author hujiahao
 */

@Slf4j
@Validated
@RestController
@Api(tags = "5. 投稿相关文件上传/下载")
@RequestMapping("/api/v1/postFiles")
public class PostFileController {
    @Resource
    PostService postService;

    @Resource
    private PostReviewerService postReviewerService;

    @PutMapping("upload")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiImplicitParam(name = "type", required = true, dataType = "int",
            value = "1稿件；2伦理委员会批文；3推荐信；4基金批文；5缴费证明")
    @ApiOperation(value = "投稿人上传稿件/推荐信/伦理委员会批文/基金批文/缴费证明")
    public AjaxResponse upload(@NotNull @RequestParam Integer id,
                               @NotNull @RequestParam Integer type,
                               @NotNull @RequestParam MultipartFile file) {

        Post post = postService.getPost(id);

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = CONTRIBUTOR_ID;

        // 检查其为稿件投稿人
        if (uid != post.getContributorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_CONTRIBUTOR);
        }

        PostStatus status = PostStatus.getItem(post.getStatus());
        String oldPath;

        PostFileType fileType = PostFileType.getItem(type);

        if (fileType == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.UNSUPPORTED_POST_FILE_TYPE);
        }

        switch (fileType) {
            case POST:
                if (status != PostStatus.TO_BE_SUBMITTED && status != PostStatus.TO_BE_REVISED && status != PostStatus.FORMAT_TO_BE_MODIFIED) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.POST_FILE_CANNOT_UPLOAD);
                }
                oldPath = post.getPostPath();
                break;
            case LETTER:
                if (status != PostStatus.TO_BE_SUBMITTED) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.LETTER_FILE_CANNOT_UPLOAD);
                }
                oldPath = post.getLetterPath();
                break;
            case ETHICS:
                if (status != PostStatus.TO_BE_SUBMITTED) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.ETHICS_FILE_CANNOT_UPLOAD);
                }
                oldPath = post.getEthicsApprovalPath();
                break;
            case FUND:
                if (status != PostStatus.TO_BE_SUBMITTED) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.FUND_FILE_CANNOT_UPLOAD);
                }
                if (post.getFundLevel() == null || post.getFundLevel() == FundLevel.NO.getCode()) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_FUND_PROJECT);
                }
                oldPath = post.getFundApprovalPath();
                break;
            case PAYMENT:
                if (status != PostStatus.CERTIFICATE_TO_BE_UPLOADED) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.PAYMENT_FILE_CANNOT_UPLOAD);
                }
                oldPath = post.getCertificatePath();
                break;
            default:
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.UNSUPPORTED_POST_FILE_TYPE);
        }

        // 新文件路径
        String path;

        if (oldPath == null) {
            // 保存文件
            path = FileUtil.FileUpload(FileUploadPath.PostFileUploadPath, file);
        } else {
            // 覆盖文件
            path = FileUtil.FileCover(FileUploadPath.PostFileUploadPath, file);
        }

        // 更新数据表
        switch (fileType) {
            case POST:
                post.setPostPath(path);
                post.setPostUploadTime(new Date());
                break;
            case LETTER:
                post.setLetterPath(path);
                post.setLetterUploadTime(new Date());
                break;
            case ETHICS:
                post.setEthicsApprovalPath(path);
                post.setEthicsApprovalUploadTime(new Date());
                break;
            case FUND:
                post.setFundApprovalPath(path);
                post.setFundApprovalUploadTime(new Date());
                break;
            case PAYMENT:
                post.setCertificatePath(path);
                post.setCertificateUploadTime(new Date());
                break;
            default:
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.UNSUPPORTED_POST_FILE_TYPE);
        }
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @GetMapping("download/role=ctr")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiImplicitParam(name = "type", required = true, dataType = "int",
            value = "1稿件；2伦理委员会批文；3推荐信；4基金批文；5缴费证明")
    @ApiOperation(value = "投稿人下载稿件/推荐信/伦理委员会批文/基金批文/缴费证明")
    public void ctrDownload(@NotNull @RequestParam Integer id,
                            @NotNull @RequestParam Integer type,
                            HttpServletResponse response,
                            HttpServletRequest request) {
        Post post = postService.getPost(id);

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = CONTRIBUTOR_ID;

        PostFileType fileType = PostFileType.getItem(type);

        if (fileType == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.UNSUPPORTED_POST_FILE_TYPE);
        }

        // 检查其为稿件投稿人
        if (uid != post.getContributorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_CONTRIBUTOR);
        }

        download(getPath(post, fileType), response, request);
    }

    @GetMapping("download/role=ed")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiImplicitParam(name = "type", required = true, dataType = "int",
            value = "1稿件；2伦理委员会批文；3推荐信；4基金批文；5缴费证明")
    @ApiOperation(value = "编辑下载稿件/推荐信/伦理委员会批文/基金批文/缴费证明")
    public void edDownload(@NotNull @RequestParam Integer id,
                           @NotNull @RequestParam Integer type,
                           HttpServletResponse response,
                           HttpServletRequest request) {
        PostFileType fileType = PostFileType.getItem(type);

        if (fileType == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.UNSUPPORTED_POST_FILE_TYPE);
        }

        Post post = postService.getPost(id);

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = EDITOR_ID;

        // 检查其为稿件编辑
        if (uid != post.getEditorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_EDITOR);
        }

        // 检查稿件状态，编辑只有在投稿人提交初审后才能下载文件
        if (post.getStatus() == PostStatus.TO_BE_SUBMITTED.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        download(getPath(post, fileType), response, request);
    }

    @GetMapping("download/role=rev")
    @PreAuthorize("hasAnyRole('ROLE_REVIEWER')")
    @ApiOperation(value = "审稿人下载稿件")
    public void revDownload(@NotNull @RequestParam Integer id,
                            HttpServletResponse response,
                            HttpServletRequest request) {
        Post post = postService.getPost(id);

        //        int uid = currentUser.getCurrentUser().getUserId();
        int uid = REVIEWER_ID;

        // 检查当前用户是否接受了审稿
        PostReviewer postReviewer = postReviewerService.getPostReviewer(id, uid);
        if (postReviewer.getAccept() != MyBoolean.TRUE.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.CANNOT_REVIEW);
        }

        // 检查稿件状态
        if (post.getStatus() != PostStatus.FIRST_REVIEW.getCode() || post.getStatus() != PostStatus.RE_REVIEW.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        download(getPath(post, PostFileType.POST), response, request);
    }


    private String getPath(Post post, PostFileType type) {
        String path;
        switch (type) {
            case POST:
                path = post.getPostPath();
                break;
            case LETTER:
                path = post.getLetterPath();
                break;
            case ETHICS:
                path = post.getEthicsApprovalPath();
                break;
            case FUND:
                path = post.getFundApprovalPath();
                break;
            case PAYMENT:
                path = post.getCertificatePath();
                break;
            default:
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.UNSUPPORTED_POST_FILE_TYPE);
        }

        if (path == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, POST_FILE_NOT_EXISTS);
        }
        return path;
    }


    private void download(String pathname, HttpServletResponse response, HttpServletRequest request) {
        response.setCharacterEncoding(request.getCharacterEncoding());
        response.setContentType("application/octet-stream");
        FileInputStream fis = null;
        try {
            File file = new File(pathname);
            if (!file.exists()) {
                throw new CustomException(CustomExceptionType.SYSTEM_ERROR, POST_FILE_NOT_EXISTS);
            } else {
                fis = new FileInputStream(file);
                response.setHeader("Content-Disposition",
                        "attachment; filename=" + URLEncoder.encode(file.getName(), StandardCharsets.UTF_8));
                IOUtils.copy(fis, response.getOutputStream());
                response.flushBuffer();
            }
        } catch (IOException ex) {
            throw new CustomException(CustomExceptionType.SYSTEM_ERROR, POST_FILE_READ_ERROR);
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (IOException ex) {
                    log.error(Arrays.toString(ex.getStackTrace()));
                }
            }
        }
    }
}

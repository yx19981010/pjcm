package com.samsph.pjcm.controller;


import com.samsph.pjcm.config.auth.CurrentUser;
import com.samsph.pjcm.config.constant.*;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.config.utils.FileUtil;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.model.PostReviewer;
import com.samsph.pjcm.service.PostReviewerService;
import com.samsph.pjcm.service.PostService;
import com.samsph.pjcm.service.UserRoleService;
import com.samsph.pjcm.service.UserService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;


import java.util.*;

import static com.samsph.pjcm.config.auth.SecurityConstants.SERVICE_URL;
import static com.samsph.pjcm.config.constant.ErrMsg.POST_FILE_NOT_EXISTS;
import static com.samsph.pjcm.config.constant.FileUploadPath.AssignmentPath;

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
            value = "1稿件；2伦理委员会批文；3推荐信；4基金批文；5缴费证明；6授权书")
    @ApiOperation(value = "投稿人上传稿件/推荐信/伦理委员会批文/基金批文/缴费证明、授权书")
    public AjaxResponse upload(@NotNull @RequestParam Integer id,
                               @NotNull @RequestParam Integer type,
                               @NotNull @RequestParam MultipartFile file) {

        Post post = postService.getPost(id);

        int uid = new CurrentUser().getCurrentUser().getUserId();
//        int uid = CONTRIBUTOR_ID;

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
                if (status != PostStatus.TO_BE_SUBMITTED && status != PostStatus.TO_BE_REVISED && status != PostStatus.FORMAT_OR_BF_PUB_TO_BE_MODIFIED) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
                }
                oldPath = post.getPostPath();
                break;
            case LETTER:
                if (status != PostStatus.TO_BE_SUBMITTED) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
                }
                oldPath = post.getLetterPath();
                break;
            case ETHICS:
                if (status != PostStatus.TO_BE_SUBMITTED) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
                }
                oldPath = post.getEthicsApprovalPath();
                break;
            case FUND:
                if (status != PostStatus.TO_BE_SUBMITTED) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
                }
                if (post.getFundLevel() == null || post.getFundLevel() == FundLevel.NO.getCode()) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_FUND_PROJECT);
                }
                oldPath = post.getFundApprovalPath();
                break;
            case PAYMENT:
                if (status != PostStatus.CERTIFICATE_TO_BE_UPLOADED) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
                }
                oldPath = post.getCertificatePath();
                break;
            case ASSIGNMENT:
                if (status != PostStatus.CERTIFICATE_TO_BE_UPLOADED) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
                }
                oldPath = post.getAssignmentPath();
                break;
            default:
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.UNSUPPORTED_POST_FILE_TYPE);
        }

        // 新文件路径
        String path;

        Date time;

        if (oldPath == null) {
            // 保存文件
            path = FileUtil.FileUpload(FileUploadPath.PostFileUploadPath, file);
        } else {
            // 覆盖文件
            path = FileUtil.FileCover(oldPath, file);
        }

        // 更新数据表
        time = new Date();
        switch (fileType) {
            case POST:
                post.setPostPath(path);
                post.setPostUploadTime(time);
                break;
            case LETTER:
                post.setLetterPath(path);
                post.setLetterUploadTime(time);
                break;
            case ETHICS:
                post.setEthicsApprovalPath(path);
                post.setEthicsApprovalUploadTime(time);
                break;
            case FUND:
                post.setFundApprovalPath(path);
                post.setFundApprovalUploadTime(time);
                break;
            case PAYMENT:
                post.setCertificatePath(path);
                post.setCertificateUploadTime(time);
                break;
            case ASSIGNMENT:
                post.setAssignmentPath(path);
                post.setAssignmentUploadTime(time);
                break;
            default:
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.UNSUPPORTED_POST_FILE_TYPE);
        }
        postService.updatePost(post);

        return AjaxResponse.success();
    }

    @GetMapping("download/role=ctr/id={id}&type={type}")
    @PreAuthorize("hasAnyRole('ROLE_CONTRIBUTOR')")
    @ApiImplicitParam(name = "type", required = true, dataType = "int",
            value = "1稿件；2伦理委员会批文；3推荐信；4基金批文；5缴费证明；6授权书；7录用通知")
    @ApiOperation(value = "投稿人下载稿件/推荐信/伦理委员会批文/基金批文/缴费证明/授权书/录用通知")
    public AjaxResponse ctrDownload(@NotNull(message = "稿件id不能为空") @Min(value = 1, message = "稿件id必须是正整数") @PathVariable(value = "id") Integer id,
                                    @NotNull(message = "稿件type不能为空") @PathVariable(value = "type") Integer type)  {

        Post post = postService.getPost(id);
        int uid = new CurrentUser().getCurrentUser().getUserId();
//        int uid = CONTRIBUTOR_ID;

        PostFileType fileType = PostFileType.getItem(type);

        if (fileType == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.UNSUPPORTED_POST_FILE_TYPE);
        }

        // 检查其为稿件投稿人
        if (uid != post.getContributorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_CONTRIBUTOR);
        }
        Map<String,String> map = new HashMap<>();
        String path = null;
        if (fileType.getCode() == 6 && post.getAssignmentPath() == null) {
            path = AssignmentPath;
            map.put("fileUrl",SERVICE_URL+"/materials"+path.substring(15));
            map.put("fileName","授权书");
        } else {
            path = getPath(post, fileType);
            map.put("fileUrl",SERVICE_URL+"/files"+path.substring(11));
            map.put("fileName",getFileName(post, fileType)+path.substring(path.lastIndexOf(".")));
        }

        return AjaxResponse.success(map);
    }

    @GetMapping("download/role=ed/id={id}&type={type}")
    @PreAuthorize("hasAnyRole('ROLE_EDITOR')")
    @ApiImplicitParam(name = "type", required = true, dataType = "int",
            value = "1稿件；2伦理委员会批文；3推荐信；4基金批文；5缴费证明；6授权书；")
    @ApiOperation(value = "编辑下载稿件/推荐信/伦理委员会批文/基金批文/缴费证明/授权书")
    public AjaxResponse edDownload(@NotNull(message = "稿件id不能为空") @Min(value = 1, message = "稿件id必须是正整数") @PathVariable(value = "id") Integer id,
                           @NotNull(message = "稿件type不能为空") @Min(value = 1, message = "类型最小值为1") @Max(value = 6, message = "类型最大值为5") @PathVariable(value = "type") Integer type) {
        PostFileType fileType = PostFileType.getItem(type);

        if (fileType == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.UNSUPPORTED_POST_FILE_TYPE);
        }

        Post post = postService.getPost(id);

        int uid = new CurrentUser().getCurrentUser().getUserId();
//        int uid = EDITOR_ID;

        // 检查其为稿件编辑
        if (uid != post.getEditorUid()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.NOT_EDITOR);
        }

        // 检查稿件状态，编辑只有在投稿人提交初审后才能下载文件
        if (post.getStatus() == PostStatus.TO_BE_SUBMITTED.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }
        Map<String,String> map = new HashMap<>();
        String path = getPath(post, fileType);
        map.put("fileUrl",SERVICE_URL+"/files"+path.substring(11));
        map.put("fileName",getFileName(post, fileType)+path.substring(path.lastIndexOf(".")));
        return AjaxResponse.success(map);
    }

    @GetMapping("download/role=rev/id={id}")
    @PreAuthorize("hasAnyRole('ROLE_REVIEWER')")
    @ApiOperation(value = "审稿人下载稿件")
    public AjaxResponse revDownload(@NotNull(message = "稿件id不能为空") @Min(value = 1, message = "稿件id必须是正整数") @PathVariable(value = "id") Integer id)   {
        Post post = postService.getPost(id);

        int uid = new CurrentUser().getCurrentUser().getUserId();
//        int uid = REVIEWER_ID;

        // 检查当前用户是否接受了审稿
        PostReviewer postReviewer = postReviewerService.getPostReviewer(id, uid);
        if (postReviewer.getAccept() != MyBoolean.TRUE.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.CANNOT_REVIEW);
        }

        // 检查稿件状态
        if (post.getStatus() != PostStatus.FIRST_REVIEW.getCode() && post.getStatus() != PostStatus.RE_REVIEW.getCode()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.WRONG_STATUS);
        }

        Map<String,String> map = new HashMap<>();
        String path = getPath(post, PostFileType.POST);
        map.put("fileUrl",SERVICE_URL+"/files"+path.substring(11));
        map.put("fileName",post.getTitle() + "_稿件_" + post.getId()+path.substring(path.lastIndexOf(".")));
        return AjaxResponse.success(map);

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
            case ASSIGNMENT:
                path = post.getAssignmentPath();
                break;
            case ACCEPTANCE:
                path = post.getAcceptanceNoticePath();
                break;
            default:
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.UNSUPPORTED_POST_FILE_TYPE);
        }

        if (path == null) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, POST_FILE_NOT_EXISTS);
        }
        return path;
    }

    private String getFileName(Post post, PostFileType type) {

        switch (type) {
            case POST:
                return post.getTitle() + "_稿件_" + post.getId();
            case LETTER:
                return post.getTitle() + "_推荐信_" + post.getId();
            case ETHICS:
                return post.getTitle() + "_伦理委员会批文_" + post.getId();
            case FUND:
                return post.getTitle() + "_基金批文_" + post.getId();
            case PAYMENT:
                return post.getTitle() + "_缴费证明_" + post.getId();
            case ASSIGNMENT:
                return post.getTitle() + "_授权转让书_" + post.getId();
            case ACCEPTANCE:
                return post.getTitle() + "_录用通知_" + post.getId();
            default:
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, ErrMsg.UNSUPPORTED_POST_FILE_TYPE);
        }
    }

}

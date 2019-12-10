package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.auth.CurrentUser;
import com.samsph.pjcm.config.constant.FileType;
import com.samsph.pjcm.config.constant.PostType;
import com.samsph.pjcm.config.constant.RoleType;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.dao.PostRepository;
import com.samsph.pjcm.model.Post;
import com.samsph.pjcm.service.CertificateService;
import com.samsph.pjcm.service.MaterialService;
import com.samsph.pjcm.service.PostService;
import com.samsph.pjcm.service.UserService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.apache.tomcat.util.http.fileupload.IOUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import java.io.*;
import java.net.URLEncoder;

@Api(tags = "文件下载")
@Validated
@RestController
@RequestMapping(value = "/api/v1")
public class FileDownloadController {
    @Autowired
    private CertificateService certificateService;
    @Autowired
    private MaterialService materialService;
    @Autowired
    private PostRepository postRepository;
    @Autowired
    private CurrentUser currentUser;

    /**
     * 下载（证书1、材料2）
     * @param id    文件id
     * @param type  文件类型
     * @param response
     * @param request
     */
    @ApiOperation(value = "下载证书，材料")
    @ApiImplicitParams({
            @ApiImplicitParam(name="id",value="文件id"),
            @ApiImplicitParam(name="type",value="文件类型")
    })
    @GetMapping(value = "/fileDownload/id={id}&type={type}")
    public void FileDownload(@NotNull(message = "id不能为空")@Min (value = 1,message = "文件id必须是正整数")@PathVariable(value = "id") Integer id,
                             @NotNull(message = "type不能为空")@Min (value = 1,message = "类型最小值为1")@Max (value = 2,message = "类型最大值为2")@PathVariable(value = "type") Integer type,
                             HttpServletResponse response,
                             HttpServletRequest request) {
        String filePath;
        switch(type){
            case FileType.CERTIFICATE: //证书
                if(certificateService.findCertificate(id).isPresent()) {
                    filePath = certificateService.findCertificate(id).get().getPath();
                    download(filePath, response, request);
                }else{
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "证书id无效或未输入证书id!!!!");
                }
                break;
            case FileType.MATERIAL://资料
                if(materialService.findMaterial(id).isPresent()) {
                    filePath = materialService.findMaterial(id).get().getPath();
                    download(filePath, response, request);
                }else{
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "材料id无效或未材料证书id!!!!");
                }
                break;
        }
    }

    /**
     * 下载（文章1、单位介绍信文件2、伦理委员会批文文件3、基金课题批文文件4、缴费证明图片5）
     * @param id
     * @param type
     * @param response
     * @param request
     */
    @ApiOperation(value = "下载文章,单位介绍信文件，伦理委员会批文文件，基金课题批文文件，缴费证明")
    @ApiImplicitParams({
            @ApiImplicitParam(name="id",value="文件id"),
            @ApiImplicitParam(name="type",value="文件类型")
    })
    @PreAuthorize("hasAnyRole('ROLE_ADMIN','ROLE_EDITOR','ROLE_REVIEWER','ROLE_CONTRIBUTOR')")
    @GetMapping(value = "/postDownload/id={id}&type={type}")
    public void AnnexDownload(@NotNull(message = "id不能为空")@Min (value = 1,message = "稿件id必须是正整数")@PathVariable(value = "id") Integer id,
                              @NotNull(message = "type不能为空")@Min (value = 1,message = "类型最小值为1")@Max (value = 5,message = "类型最大值为5")@PathVariable(value = "type") Integer type,
                              HttpServletResponse response,
                              HttpServletRequest request) {
        if(!postRepository.findById(id).isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "文章id无效或未输入文章id!!!!");
        }
        Post post = postRepository.findById(id).get();
//        int role = currentUser.getCurrentUser().getUserRole();
//        int userId = currentUser.getCurrentUser().getUserId();
//        if(role == RoleType.CONTRIBUTOR_ROLE){
//            if(post.getContributorUid() != userId){
//                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "无权查看其他投稿人的文件信息");
//            }
//        }
//        if(role == RoleType.REVIEWER_ROLE){
//            if(type != 1){
//                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "无权查看投稿人的其他文件信息");
//            }
//        }
        String filePath;
        switch(type){
            case PostType.POST: //文章
                filePath = post.getPostPath();
                download(filePath, response, request);
                break;
            case PostType.LETTER: //单位介绍信文件
                filePath = post.getLetterPath();
                download(filePath, response, request);
                break;
            case PostType.ETHICS: //伦理委员会批文文件
                filePath = post.getEthicsApprovalPath();
                download(filePath, response, request);
                break;
            case PostType.FUND: //基金课题批文文件
                filePath = post.getFundApprovalPath();
                download(filePath, response, request);
                break;
            case PostType.PAYMENT: //缴费证明
                filePath = post.getCertificatePath();
                download(filePath, response, request);
                break;
        }
    }


    /**
     * 文件下载
     * @param pathname
     * @param response
     * @param request
     */
    private void download(String pathname,HttpServletResponse response, HttpServletRequest request){
        response.setCharacterEncoding(request.getCharacterEncoding());
        response.setContentType("application/octet-stream");
        FileInputStream fis = null;
        try{
            File file = new File(pathname);
            if(!file.exists()){
                response.setStatus(404);
            }
            else{
                fis = new FileInputStream(file);
                response.setHeader("Content-Disposition", "attachment; filename=" + URLEncoder.encode(file.getName(), "UTF-8"));
                IOUtils.copy(fis, response.getOutputStream());
                response.flushBuffer();
            }
        }
        catch(IOException e1){
            e1.printStackTrace();
            throw new CustomException(CustomExceptionType.SYSTEM_ERROR,"文件读取异常!!!");
        }
        finally{
            if(fis != null){
                try{
                    fis.close();
                }
                catch(IOException e){
                    e.printStackTrace();
                }
            }
        }
    }
}


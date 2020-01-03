package com.samsph.pjcm.controller;

import com.samsph.pjcm.config.PageData;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.config.constant.FileUploadPath;
import com.samsph.pjcm.model.Certificate;
import com.samsph.pjcm.service.CertificateService;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.config.utils.FileUtil;
import com.samsph.pjcm.vo.CertificateVoGet;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.sql.Date;
import java.util.List;

@Api(tags = "证书管理")
@Validated
@RestController
@RequestMapping(value = "/api/v1")
public class CertificatesController {
    @Autowired
    private CertificateService certificateService;

    @ApiOperation(value = "添加证书")
    @PostMapping("/certificates")
    @ApiImplicitParams({
            @ApiImplicitParam(name="name",value="证书名"),
            @ApiImplicitParam(name="description",value="证书描述"),
            @ApiImplicitParam(name="filename",value="证书图片")
    })
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse addCertificate(@Size(min = 1,max = 50) @RequestParam("name") String name,
                                       @Size(min = 1,max = 100) @RequestParam(name = "description",required = false) String description,
                                       @RequestParam("filename") MultipartFile file) {
        //权限检测 用户身份是管理员且登录
        String pathname = FileUtil.FileUpload(FileUploadPath.CertificateFileUploadPath, file);
        Date time = new Date(new java.util.Date().getTime());
        Certificate certificate = new Certificate();
        //定义为登录后管理员的id
//        certificate.setCreateByUid(currentUser.getCurrentUser().getUserId());
        certificate.setCreateByUid(7);
        certificate.setCreateTime(time);
        certificate.setDescription(description);
        certificate.setName(name);
        certificate.setPath(pathname);
        certificateService.addCertificate(certificate);
        return AjaxResponse.success();
    }

    @ApiOperation(value = "更新证书")
    @ApiImplicitParams({
            @ApiImplicitParam(name="id",value="证书id"),
            @ApiImplicitParam(name="name",value="证书名"),
            @ApiImplicitParam(name="description",value="证书描述"),
            @ApiImplicitParam(name="filename",value="证书图片")
    })
    @PutMapping("/certificates")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse updateCertificate(@NotNull(message = "未传入证书id")@Min(value = 1,message = "证书id必须是正整数") @RequestParam("id") Integer id,
                                          @Size(min = 1,max = 50) @RequestParam(value = "name",required = false) String name,
                                          @Size(min = 1,max = 100) @RequestParam(value = "description",required = false) String description,
                                          @RequestParam(value = "filename",required = false) MultipartFile file) {
        //权限检测 用户身份是管理员且登录
        //id为空或者id不存在
        if (!certificateService.findCertificate(id).isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "证书id无效或未输入证书id!!!!");
        } else {
            Certificate certificate = certificateService.findCertificate(id).get();
            if(file != null){
                String path = certificate.getPath();
                String newPath = FileUtil.FileCover(path,file);
                certificate.setPath(newPath);
            }
            if(name != null){
                certificate.setName(name);
            }
            if(description != null){
                certificate.setDescription(description);
            }
            certificateService.updateCertificate(certificate);
            return AjaxResponse.success();
        }
    }

    @ApiOperation(value = "删除证书")
    @ApiImplicitParams({
            @ApiImplicitParam(name="id",value="证书id")
    })
    @DeleteMapping("/certificates/id={id}")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse deleteCertificate(@NotNull(message = "id不能为空")@Min(value = 1,message = "证书id必须是正整数")@PathVariable Integer id) {
        //权限检测 用户身份是管理员且登录
        //id为空或者id不存在
        if (!certificateService.findCertificate(id).isPresent()) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "证书id无效或未输入证书id!!!!");
        } else {
            FileUtil.deleteFile(certificateService.findCertificate(id).get().getPath());
            certificateService.deleteCertificate(id);
            return AjaxResponse.success();
        }
    }

    @ApiOperation(value = "得到单个证书")
    @ApiImplicitParams({
            @ApiImplicitParam(name="id",value="证书id")
    })
    @GetMapping("/certificates/id={id}")
    public AjaxResponse findCertificate(@NotNull(message = "id不能为空")@Min(value = 1,message = "证书id必须是正整数")@PathVariable Integer id) {
        if (certificateService.findCertificate(id).isPresent()) {
            Certificate certificate = certificateService.findCertificate(id).get();
            return AjaxResponse.success(DozerUtil.map(certificate, CertificateVoGet.class));
        } else {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "证书id无效或未输入证书id!!!!");
        }
    }

    @ApiOperation(value = "得到证书列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name="page",value="页数"),
            @ApiImplicitParam(name="size",value="大小")
    })
    @GetMapping("/certificates")
    public AjaxResponse findCertificates(@NotNull(message = "未传入页数")@Min(value = 1,message = "页数必须是正整数")@RequestParam("page") Integer page,
                                         @NotNull(message = "未传入每页的大小")@Min(value = 1,message = "每页的大小必须是正整数")@RequestParam("size") Integer size) {
        PageRequest pageRequest = PageRequest.of(page-1, size, Sort.Direction.DESC, "createTime");
        Page<Certificate> certificatePage = certificateService.findCertificates(pageRequest);
        List<Certificate> certificateList = certificatePage.getContent();
        List<CertificateVoGet> certificateVoGets = DozerUtil.mapList(certificateList, CertificateVoGet.class);
        PageData pageData = new PageData(certificatePage.getTotalPages(), (int) certificatePage.getTotalElements(),page, certificateVoGets.size(), certificateVoGets);
        return AjaxResponse.success(pageData);
    }
}





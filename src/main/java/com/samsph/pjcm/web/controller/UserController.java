package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.PageData;
import com.samsph.pjcm.config.auth.CurrentUser;
import com.samsph.pjcm.config.constant.RoleType;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.model.ReviewerField;
import com.samsph.pjcm.model.User;
import com.samsph.pjcm.model.UserRole;
import com.samsph.pjcm.service.*;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.config.utils.Sha256Util;
import com.samsph.pjcm.config.utils.UUIDUtil;
import com.samsph.pjcm.vo.*;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.parameters.P;
import org.springframework.security.web.authentication.logout.SecurityContextLogoutHandler;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import javax.validation.constraints.*;
import java.util.*;

@Api(tags = "用户管理")
@RestController
@Validated
@RequestMapping(value = "/api/v1")
public class UserController {
    @Autowired
    private UserService userService;
    @Autowired
    private UserRoleService userRoleService;
    @Autowired
    private MailService mailService;
    @Autowired
    private EditorFieldService editorFieldService;
    @Autowired
    private ReviewerFieldService reviewerFieldService;

    @ApiOperation(value = "添加用户")
    @PostMapping("/users")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse addUser(@Valid @RequestBody UserERVoPost userERVoPost){
        Integer role = userERVoPost.getRoleId();
        if (role == RoleType.CONTRIBUTOR_ROLE) {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"不能添加作者只能注册添加");
        }
        if(userService.findUserByEmail(userERVoPost.getEmail()).isPresent()){
            User user1 = userService.findUserByEmail(userERVoPost.getEmail()).get();
            if(user1.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"账户待激活或已注销无法添加!!!");
            }else{
                if(userRoleService.findUserHasRole(user1.getId(),role)) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "该邮箱已被注册");
                }else{
                    UserRole userRole = new UserRole();
                    userRole.setUid(user1.getId());
                    userRole.setRole(role);
                    userRoleService.addUserRole(userRole);
                    return AjaxResponse.success();
                }
            }
        }else {
            userService.addUser(userERVoPost);
            Integer id = userService.findUserByEmail(userERVoPost.getEmail()).get().getId();
            UserRole userRole = new UserRole();
            userRole.setUid(id);
            userRole.setRole(role);
            userRoleService.addUserRole(userRole);
            return AjaxResponse.success();
        }
    }

    @ApiOperation(value = "用户激活")
    @ApiImplicitParams({
            @ApiImplicitParam(name="code",value="激活码")
    })
    @GetMapping("/users/checkCode")
    public AjaxResponse activeUser(@RequestParam("code") String code){
        if(userService.findUserByCode(code).isPresent()){
            User user = userService.findUserByCode(code).get();
            user.setActive(1);
            user.setCode("");
            userService.updateUser(user);
            return AjaxResponse.success();
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"code错误!!!");
        }
    }

    @ApiOperation(value = "修改邮箱激活")
    @ApiImplicitParams({
            @ApiImplicitParam(name="code",value="激活码"),
            @ApiImplicitParam(name="email",value="邮箱")
    })
    @GetMapping("/users/checkCodeForNewEmail")
    public AjaxResponse checkCodeForNewEmail(@RequestParam("code") String code,@Email @RequestParam("email") String email){
        if(userService.findUserByCode(code).isPresent()){
            User user = userService.findUserByCode(code).get();
            String code1 = UUIDUtil.getUUID()+ UUIDUtil.getUUID();
            String password = UUIDUtil.getPasswordUUID();
            user.setPasswordHash(Sha256Util.getSHA256StrJava(password));
            user.setEmail(email);
            user.setActive(0);
            user.setCode(code1);
            userService.updateUser(user);
            mailService.sendHtmlMailForNewEmailActive(email,code1,password);
            return AjaxResponse.success();
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"code错误!!!");
        }
    }

    @ApiOperation(value = "修改密码激活")
    @ApiImplicitParams({
            @ApiImplicitParam(name="code",value="激活码")
    })
    @GetMapping("/users/checkCodeForNewPassword")
    public AjaxResponse checkCodeForNewPassword(@RequestParam("code") String code){
        String newPassword = code.substring(code.length()-8,code.length());
        String code1 = code.substring(0,code.length()-8);
        if(userService.findUserByCode(code1).isPresent()){
            User user = userService.findUserByCode(code1).get();
            user.setPasswordHash(Sha256Util.getSHA256StrJava(newPassword));
            user.setCode("");
            userService.updateUser(user);
            return AjaxResponse.success();
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"code错误!!!");
        }
    }

    @ApiOperation(value = "检查邮箱是否已被注册——只为注册提供")
    @ApiImplicitParams({
            @ApiImplicitParam(name="email",value="邮箱")
    })
    @GetMapping("/users/checkEmail")
    public AjaxResponse checkEmail(@RequestParam("email") String email){
        Map<String, Object> map = new HashMap<>();
        if(userService.findUserByEmail(email).isPresent()){
            User user = userService.findUserByEmail(email).get();
            if(userRoleService.findUserHasRole(user.getId(),RoleType.CONTRIBUTOR_ROLE)){
                map.put("result",1);
                map.put("reason","邮箱存在");
                return AjaxResponse.success(map);
            }
        }
        map.put("result",0);
        map.put("reason","邮箱不存在");
        return AjaxResponse.success(map);
    }


    @ApiOperation(value = "用户忘记密码")
    @ApiImplicitParams({
            @ApiImplicitParam(name="email",value="邮箱")
    })
    @GetMapping("/users/forgetPassword")
    public AjaxResponse forgetPassword( @Email @RequestParam("email") String email){
        if(!userService.findUserByEmail(email).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户不存在");
        }else{
            User user = userService.findUserByEmail(email).get();
            if(user.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活或已注销");
            }else {
                String code = UUIDUtil.getUUID();
                String password = UUIDUtil.getPasswordUUID();
                user.setCode(code);
                userService.updateUser(user);
                mailService.sendHtmlMailForChangePassword(email,code,password);
                return AjaxResponse.success();
            }
        }
    }

    @ApiOperation(value = "用户修改密码成功修改后返回首页")
    @ApiImplicitParams({
            @ApiImplicitParam(name="oldPassword",value="原密码"),
            @ApiImplicitParam(name="newPassword",value="新密码"),
            @ApiImplicitParam(name="id",value="用户id")
    })
    @PostMapping("/users/changePassword")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN','ROLE_EDITOR','ROLE_REVIEWER','ROLE_CONTRIBUTOR')")
    public AjaxResponse changePassword(@Size(min = 8,max = 20)@RequestParam("oldPassword") String oldPassword,
                                       @Size(min = 8,max = 20)@RequestParam("newPassword") String newPassword,
                                       @NotNull(message = "未传入用户id")@Min(value = 1,message = "用户id必须是正整数") @RequestParam("id") Integer id,
                                       HttpServletRequest request,
                                       HttpServletResponse response){
        //用户登录检测
//        if(currentUser.getCurrentUser().getUserId() != id){
//            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"无权修改其他人的密码");
//        }
        if(!userService.findUserByUid(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户不存在");
        }else{
            User user = userService.findUserByUid(id).get();
            if(user.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活或已注销");
            }else {
                if (user.getPasswordHash().equals(Sha256Util.getSHA256StrJava(oldPassword))) {
                    user.setPasswordHash(Sha256Util.getSHA256StrJava(newPassword));
                    userService.updateUser(user);
                    Authentication auth = SecurityContextHolder.getContext().getAuthentication();
                    if(auth != null){
                        new SecurityContextLogoutHandler().logout(request,response,auth);
                    }
                    return AjaxResponse.success();
                } else {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "原密码错误");
                }
            }
        }
    }

    @ApiOperation(value = "用户修改邮箱修改后返回首页")
    @ApiImplicitParams({
            @ApiImplicitParam(name="newEmail",value="新邮箱"),
            @ApiImplicitParam(name="id",value="用户id")
    })
    @PostMapping("/users/changeEmail")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN','ROLE_EDITOR','ROLE_REVIEWER','ROLE_CONTRIBUTOR')")
    public AjaxResponse changeEmail( @Email @RequestParam("newEmail") String newEmail,
                                     @NotNull(message = "未传入用户id")@Min(value = 1,message = "用户id必须是正整数") @RequestParam("id") Integer id){
        //用户登录检测
//        if(currentUser.getCurrentUser().getUserId() != id){
//            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"无权修改其他人的邮箱");
//        }
        if(!userService.findUserByUid(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户不存在");
        }else{
            User user = userService.findUserByUid(id).get();
            if(user.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活或已注销");
            }else {
                    if(userService.findUserByEmail(newEmail).isPresent()){
                        throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"邮箱已注册");
                    }else{
                        String code = UUIDUtil.getUUID()+ UUIDUtil.getUUID();
                        user.setCode(code);
                        userService.updateUser(user);
                        mailService.sendHtmlMailForChangeEmail(user.getEmail(),code,newEmail);
                        //返回首页
                        return AjaxResponse.success();
                    }
            }
        }
    }

    @ApiOperation(value = "重置密码")
    @ApiImplicitParams({
            @ApiImplicitParam(name="id",value="用户id"),
            @ApiImplicitParam(name="newPassword",value="新密码")
    })
    @PostMapping("/users/resetPassword")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse resetPassword( @Size(min = 8,max = 20)@RequestParam("newPassword") String newPassword,
                                       @NotNull(message = "未传入用户id")@Min(value = 1,message = "用户id必须是正整数") @RequestParam("id") Integer id){
        //管理员登录检测
        if(!userService.findUserByUid(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户不存在");
        }else{
            User user = userService.findUserByUid(id).get();
            if(user.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活或已注销");
            }else {
                user.setPasswordHash(Sha256Util.getSHA256StrJava(newPassword));
                userService.updateUser(user);
                return AjaxResponse.success();
            }
        }
    }

    @ApiOperation(value = "重置邮箱")
    @ApiImplicitParams({
            @ApiImplicitParam(name="id",value="用户id"),
            @ApiImplicitParam(name="email",value="新邮箱")
    })
    @PostMapping("/users/resetEmail")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse resetEmail(@NotNull(message = "未传入用户id")@Min(value = 1,message = "用户id必须是正整数") @RequestParam("id") Integer id,
                                   @Email @RequestParam("email") String email){
        //管理员登录检测
        if(!userService.findUserByUid(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户不存在");
        }else{
            User user = userService.findUserByUid(id).get();
            if(user.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活或已注销");
            }else {
                if (!userService.findUserByEmail(email).isPresent()) {
                    String password = UUIDUtil.getPasswordUUID();
                    user.setPasswordHash(Sha256Util.getSHA256StrJava(password));
                    user.setCode(UUIDUtil.getUUID() + UUIDUtil.getUUID());
                    user.setActive(0);
                    user.setEmail(email);
                    userService.updateUser(user);
                    mailService.sendHtmlMailForNewEmailActive(email,user.getCode(),password);
                    return AjaxResponse.success();
                } else {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "邮箱已注册");
                }
            }
        }
    }

    @ApiOperation(value = "修改用户信息")
    @PutMapping("/users")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN','ROLE_EDITOR','ROLE_REVIEWER','ROLE_CONTRIBUTOR')")
    public AjaxResponse updateUser(@Valid @RequestBody UserVoPut userVoPut){
        //用户登录检测
//        if(currentUser.getCurrentUser().getUserId() != userVoPut.getId()){
//            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"无权修改其他人的信息");
//        }
        if(!userService.findUserByUid(userVoPut.getId()).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户不存在");
        }else{
            User user = userService.findUserByUid(userVoPut.getId()).get();
            if(user.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活或已注销");
            }else {
                if (userVoPut.getAddress() != null) {
                    user.setAddress(userVoPut.getAddress());
                }
                if (userVoPut.getBankAccount() != null) {
                    user.setBankAccount(userVoPut.getBankAccount());
                }
                if (userVoPut.getBankName() != null) {
                    user.setBankName(userVoPut.getBankName());
                }
                if (userVoPut.getEducation() != null) {
                    user.setEducation(userVoPut.getEducation());
                }
                if (userVoPut.getEmployer() != null) {
                    user.setEmployer(userVoPut.getEmployer());
                }
                if (userVoPut.getMajor() != null) {
                    user.setMajor(userVoPut.getMajor());
                }
                if (userVoPut.getPhone() != null) {
                    user.setPhone(userVoPut.getPhone());
                }
                if (userVoPut.getSex() != null) {
                    user.setSex(userVoPut.getSex());
                }
                if (userVoPut.getTitle() != null) {
                    user.setTitle(userVoPut.getTitle());
                }
                if (userVoPut.getUserName() != null) {
                    user.setUserName(userVoPut.getUserName());
                }
                if (userVoPut.getZipCode() != null) {
                    user.setZipCode(userVoPut.getZipCode());
                }
                userService.updateUser(user);
                return AjaxResponse.success();
            }
        }
    }

    @ApiOperation(value = "注销用户")
    @ApiImplicitParams({
            @ApiImplicitParam(name="uid",value="用户id"),
    })
    @DeleteMapping("/users/uid={uid}")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse deleteUser(@NotNull(message = "uid不能为空")@Min(value = 1,message = "uid必须是正整数") @PathVariable Integer uid){
        //管理员检测
        if(userService.findUserByUid(uid).isPresent()){
            if(userService.findUserByUid(uid).get().getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活或已注销");
            }else {
                userService.deleteUser(uid);
                return AjaxResponse.success();
            }
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"用户id输入错误!!!");
        }
    }

    @ApiOperation(value = "重新激活用户(对于已注销的用户)")
    @ApiImplicitParams({
            @ApiImplicitParam(name="uid",value="用户id"),
    })
    @PutMapping("/users/uid={uid}")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse ActiveUser(@NotNull(message = "uid不能为空")@Min(value = 1,message = "uid必须是正整数") @PathVariable Integer uid){
        //管理员检测
        if(userService.findUserByUid(uid).isPresent()){
            if(userService.findUserByUid(uid).get().getActive() != -1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未注销");
            }else {
                User user = userService.findUserByUid(uid).get();
                user.setActive(1);
                userService.updateUser(user);
                return AjaxResponse.success();
            }
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"用户id输入错误!!!");
        }
    }

    @ApiOperation(value = "得到用户个人信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name="uid",value="用户id"),
            @ApiImplicitParam(name="rid",value="角色id")
    })
    @GetMapping("/users/uid={uid}&rid={rid}")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN','ROLE_EDITOR','ROLE_REVIEWER','ROLE_CONTRIBUTOR')")
    public AjaxResponse findUserByUidAndRid(@NotNull(message = "uid不能为空")@Min(value = 1,message = "uid必须是正整数") @PathVariable Integer uid,
                                            @NotNull(message = "rid不能为空")@Min(value = RoleType.ADMIN_ROLE)@Max(value = RoleType.CONTRIBUTOR_ROLE) @PathVariable Integer rid){
//        if(currentUser.getCurrentUser().getUserRole() != RoleType.ADMIN_ROLE) {
//            if (currentUser.getCurrentUser().getUserId() != uid || currentUser.getCurrentUser().getUserRole() != rid) {
//                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "无权查看其他人的信息");
//            }
//        }
        Optional<User> userOptional = userService.findUserByUid(uid);
        if(userOptional.isPresent()) {
            if(!userRoleService.findUserHasRole(uid,rid)){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户没有此角色或者角色未激活或者角色已注销!!!");
            }else {
                switch (rid) {
                    case RoleType.ADMIN_ROLE:
                        UserAdminVoGet userAdminVoGet = DozerUtil.map(userOptional.get(), UserAdminVoGet.class);
                        return AjaxResponse.success(userAdminVoGet);
                    case RoleType.EDITOR_ROLE:
                        UserEditorVoGet userEditorVoGet = new UserEditorVoGet();
                        List<EditorField> list1 = editorFieldService.findByEditorUid(uid);
                        List<EditorFieldVoGetField> list2 = DozerUtil.mapList(list1,EditorFieldVoGetField.class);
                        userEditorVoGet.setActive(userOptional.get().getActive());
                        userEditorVoGet.setEmail(userOptional.get().getEmail());
                        userEditorVoGet.setId(userOptional.get().getId());
                        userEditorVoGet.setPhone(userOptional.get().getPhone());
                        userEditorVoGet.setSex(userOptional.get().getSex());
                        userEditorVoGet.setUserName(userOptional.get().getUserName());
                        userEditorVoGet.setField(list2);
                        return AjaxResponse.success(userEditorVoGet);
                    case RoleType.REVIEWER_ROLE:
                        UserReviewerVoGet userReviewerVoGet = new UserReviewerVoGet();
                        List<ReviewerField> list3 = reviewerFieldService.findByReviewerUid(uid);
                        List<ReviewerFieldVoGetField> list4 = DozerUtil.mapList(list3,ReviewerFieldVoGetField.class);
                        userReviewerVoGet.setEmail(userOptional.get().getEmail());
                        userReviewerVoGet.setActive(userOptional.get().getActive());
                        userReviewerVoGet.setId(userOptional.get().getId());
                        userReviewerVoGet.setPhone(userOptional.get().getPhone());
                        userReviewerVoGet.setSex(userOptional.get().getSex());
                        userReviewerVoGet.setUserName(userOptional.get().getUserName());
                        userReviewerVoGet.setField(list4);
                        userReviewerVoGet.setBankAccount(userOptional.get().getBankAccount());
                        userReviewerVoGet.setBankName(userOptional.get().getBankName());
                        return AjaxResponse.success(userReviewerVoGet);
                    case RoleType.CONTRIBUTOR_ROLE:
                        UserContributorVoGet userContributorVoGet = DozerUtil.map(userOptional.get(), UserContributorVoGet.class);
                        return AjaxResponse.success(userContributorVoGet);
                    default:
                        throw new CustomException(CustomExceptionType.OTHER_ERROR, "系统出现未知错误");
                }
            }
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"uid无效或uid输入错误!!");
        }
    }

    @ApiOperation(value = "得到某个角色的所有用户")
    @ApiImplicitParams({
            @ApiImplicitParam(name="rid",value="角色id"),
            @ApiImplicitParam(name="page",value="页数"),
            @ApiImplicitParam(name="size",value="大小")
    })
    @GetMapping("/users")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse findUsersByRoleId(@NotNull(message = "id不能为空")@Min(value = RoleType.ADMIN_ROLE)@Max(value = RoleType.CONTRIBUTOR_ROLE) @RequestParam("rid") Integer rid,
                                           @NotNull(message = "未传入页数")@Min(value = 1,message = "页数必须是正整数") @RequestParam("page") Integer page,
                                           @NotNull(message = "未传入每页的大小")@Min(value = 1,message = "每页的大小必须是正整数") @RequestParam("size") Integer size){
        PageRequest pageRequest = PageRequest.of(page-1,size);
        Page<User> userPage = userService.findUsersByRid(rid,pageRequest);
        List<User> userList = userPage.getContent();
        PageData pageData;
        if(userList != null){
            switch (rid){
                case RoleType.ADMIN_ROLE:
                    List<UserAdminVoGet> userAdminVoGets = DozerUtil.mapList(userList, UserAdminVoGet.class);
                    pageData = new PageData(userPage.getTotalPages(), (int) userPage.getTotalElements(),page,userAdminVoGets.size(),userAdminVoGets);
                    return AjaxResponse.success(pageData);
                case RoleType.EDITOR_ROLE:
                    List<UserEditorVoGet> userEditorVoGets = new ArrayList<>();
                    for(User user : userList){
                        List<EditorField> list1 = editorFieldService.findByEditorUid(user.getId());
                        List<EditorFieldVoGetField> list2 = DozerUtil.mapList(list1,EditorFieldVoGetField.class);
                        UserEditorVoGet userEditorVoGet = new UserEditorVoGet();
                        userEditorVoGet.setActive(user.getActive());
                        userEditorVoGet.setEmail(user.getEmail());
                        userEditorVoGet.setId(user.getId());
                        userEditorVoGet.setPhone(user.getPhone());
                        userEditorVoGet.setSex(user.getSex());
                        userEditorVoGet.setUserName(user.getUserName());
                        userEditorVoGet.setField(list2);
                        userEditorVoGets.add(userEditorVoGet);
                    }
                    pageData = new PageData(userPage.getTotalPages(), (int) userPage.getTotalElements(),page,userEditorVoGets.size(),userEditorVoGets);
                    return AjaxResponse.success(pageData);
                case RoleType.REVIEWER_ROLE:
                    List<UserReviewerVoGet> userReviewerVoGets = new ArrayList<>();
                    for(User user : userList){
                        List<ReviewerField> list1 = reviewerFieldService.findByReviewerUid(user.getId());
                        List<ReviewerFieldVoGetField> list2 = DozerUtil.mapList(list1,ReviewerFieldVoGetField.class);
                        UserReviewerVoGet userReviewerVoGet = new UserReviewerVoGet();
                        userReviewerVoGet.setEmail(user.getEmail());
                        userReviewerVoGet.setActive(user.getActive());
                        userReviewerVoGet.setId(user.getId());
                        userReviewerVoGet.setPhone(user.getPhone());
                        userReviewerVoGet.setSex(user.getSex());
                        userReviewerVoGet.setUserName(user.getUserName());
                        userReviewerVoGet.setField(list2);
                        userReviewerVoGet.setBankAccount(user.getBankAccount());
                        userReviewerVoGet.setBankName(user.getBankName());
                        userReviewerVoGets.add(userReviewerVoGet);
                    }
                    pageData = new PageData(userPage.getTotalPages(), (int) userPage.getTotalElements(),page,userReviewerVoGets.size(),userReviewerVoGets);
                    return AjaxResponse.success(pageData);
                case RoleType.CONTRIBUTOR_ROLE:
                    List<UserContributorVoGet> userContributorVoGets = DozerUtil.mapList(userList,UserContributorVoGet.class);
                    pageData = new PageData(userPage.getTotalPages(), (int) userPage.getTotalElements(),page,userContributorVoGets.size(),userContributorVoGets);
                    return AjaxResponse.success(pageData);
                default:
                    throw new CustomException(CustomExceptionType.OTHER_ERROR,"系统出现未知错误");
            }
        }else {
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"系统无对应角色的用户!!!");
        }
    }
}

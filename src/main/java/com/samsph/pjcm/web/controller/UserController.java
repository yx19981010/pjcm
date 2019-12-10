package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.PageData;
import com.samsph.pjcm.config.constant.RoleType;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.model.User;
import com.samsph.pjcm.model.UserRole;
import com.samsph.pjcm.service.MailService;
import com.samsph.pjcm.service.UserRoleService;
import com.samsph.pjcm.service.UserService;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.config.utils.Sha256Util;
import com.samsph.pjcm.config.utils.UUIDUtil;
import com.samsph.pjcm.vo.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

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

    @PostMapping("/users")
    @PreAuthorize("hasAnyRole('ROLE_1')")
    public AjaxResponse addUser(@Valid @RequestBody UserVoPost userVoPost){
        Integer role = userVoPost.getRoleId();
        if(userService.findUserByEmail(userVoPost.getEmail()).isPresent()){
            User user1 = userService.findUserByEmail(userVoPost.getEmail()).get();
            if(user1.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"账户待激活或已注销无法添加!!!");
            }else{
                if(userRoleService.findUserHasRole(user1.getId(),role)) {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "该邮箱已被注册!!!");
                }else{
                    UserRole userRole = new UserRole();
                    userRole.setUid(user1.getId());
                    userRole.setRole(role);
                    userRoleService.addUserRole(userRole);
                    return AjaxResponse.success();
                }
            }
        }else {
            if (role == RoleType.CONTRIBUTOR_ROLE) {
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"不能添加作者");
            } else {
                userService.addUser(userVoPost);
            }
            Integer id = userService.findUserByEmail(userVoPost.getEmail()).get().getId();
            UserRole userRole = new UserRole();
            userRole.setUid(id);
            userRole.setRole(role);
            userRoleService.addUserRole(userRole);
            return AjaxResponse.success();
        }
    }

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

    @GetMapping("/users/checkEmail")
    public AjaxResponse checkEmail(@RequestParam("email") String email){
        Map<String, Object> map = new HashMap<>();
        if(userService.findUserByEmail(email).isPresent()){
            map.put("result",1);
            map.put("reason","邮箱存在");
            return AjaxResponse.success(map);
        }else{
            map.put("result",0);
            map.put("reason","邮箱不存在");
            return AjaxResponse.success(map);
        }
    }

    @PostMapping("/users/changePassword")
    @PreAuthorize("hasAnyRole('ROLE_1','ROLE_2','ROLE_3','ROLE_4')")
    public AjaxResponse changePassword( @Size(min = 8,max = 20)@RequestParam("oldPassword") String oldPassword,
                                        @Size(min = 8,max = 20)@RequestParam("newPassword") String newPassword,
                                        @NotNull(message = "未传入用户id")@Min(value = 1,message = "用户id必须是正整数") @RequestParam("id") Integer id){
        //用户登录检测
        if(!userService.findUserByUid(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户不存在");
        }else{
            User user = userService.findUserByUid(id).get();
            if(user.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活");
            }else {
                if (user.getPasswordHash().equals(Sha256Util.getSHA256StrJava(oldPassword))) {
                    user.setPasswordHash(Sha256Util.getSHA256StrJava(newPassword));
                    userService.updateUser(user);
                    return AjaxResponse.success();
                } else {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "原密码错误");
                }
            }
        }
    }

    @PostMapping("/users/changeEmail")
    @PreAuthorize("hasAnyRole('ROLE_1','ROLE_2','ROLE_3','ROLE_4')")
    public AjaxResponse changeEmail( @Email @RequestParam("oldEmail") String oldEmail,
                                     @Email @RequestParam("newEmail") String newEmail,
                                     @NotNull(message = "未传入用户id")@Min(value = 1,message = "用户id必须是正整数") @RequestParam("id") Integer id){
        //用户登录检测
        if(!userService.findUserByUid(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户不存在");
        }else{
            User user = userService.findUserByUid(id).get();
            if(user.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活");
            }else {
                if(!oldEmail.equals(user.getEmail())){
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"原邮箱输入错误");
                }else{
                    if(userService.findUserByEmail(newEmail).isPresent()){
                        throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"邮箱已注册");
                    }else{
                        String code = UUIDUtil.getUUID()+ UUIDUtil.getUUID();
                        user.setCode(code);
                        user.setActive(0);
                        user.setEmail(newEmail);
                        userService.updateUser(user);
                        mailService.sendHtmlMailForActive(newEmail,code);
                        return AjaxResponse.success();
                    }
                }
            }
        }
    }

    @PostMapping("/users/resetPassword")
    @PreAuthorize("hasAnyRole('ROLE_1')")
    public AjaxResponse resetPassword(@NotNull(message = "未传入用户id")@Min(value = 1,message = "用户id必须是正整数") @RequestParam("id") Integer id){
        //管理员登录检测
        if(!userService.findUserByUid(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户不存在");
        }else{
            User user = userService.findUserByUid(id).get();
            if(user.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活");
            }else {
                user.setPasswordHash(Sha256Util.getSHA256StrJava("12345678"));
                userService.updateUser(user);
                return AjaxResponse.success();
            }
        }
    }

    @PostMapping("/users/resetEmail")
    @PreAuthorize("hasAnyRole('ROLE_1')")
    public AjaxResponse resetEmail(@NotNull(message = "未传入用户id")@Min(value = 1,message = "用户id必须是正整数") @RequestParam("id") Integer id,
                                   @Email @RequestParam("email") String email){
        //管理员登录检测
        if(!userService.findUserByUid(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户不存在");
        }else{
            User user = userService.findUserByUid(id).get();
            if(user.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活");
            }else {
                if (!userService.findUserByEmail(email).isPresent()) {
                    String code = UUIDUtil.getUUID() + UUIDUtil.getUUID();
                    user.setCode(code);
                    user.setActive(0);
                    user.setEmail(email);
                    userService.updateUser(user);
                    mailService.sendHtmlMailForActive(email,code);
                    return AjaxResponse.success();
                } else {
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR, "邮箱已注册");
                }
            }
        }
    }

    @PutMapping("/users")
    @PreAuthorize("hasAnyRole('ROLE_1','ROLE_2','ROLE_3','ROLE_4')")
    public AjaxResponse updateUser(@Valid @RequestBody UserVoPut userVoPut){
        //用户登录检测
        if(!userService.findUserByUid(userVoPut.getId()).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户不存在");
        }else{
            User user = userService.findUserByUid(userVoPut.getId()).get();
            if(user.getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活");
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

    @DeleteMapping("/users/uid={uid}")
    @PreAuthorize("hasAnyRole('ROLE_1')")
    public AjaxResponse deleteUser(@NotNull(message = "uid不能为空")@Min(value = 1,message = "uid必须是正整数") @PathVariable Integer uid){
        //管理员检测
        if(userService.findUserByUid(uid).isPresent()){
            if(userService.findUserByUid(uid).get().getActive() != 1){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户未激活");
            }else {
                userService.deleteUser(uid);
                return AjaxResponse.success();
            }
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"用户id输入错误!!!");
        }
    }

    @GetMapping("/users/uid={uid}&rid={rid}")
    @PreAuthorize("hasAnyRole('ROLE_1','ROLE_2','ROLE_3','ROLE_4')")
    public AjaxResponse findUserByUidAndRid(@NotNull(message = "uid不能为空")@Min(value = 1,message = "uid必须是正整数") @PathVariable Integer uid,
                                            @NotNull(message = "rid不能为空")@Min(value = RoleType.ADMIN_ROLE)@Max(value = RoleType.CONTRIBUTOR_ROLE) @PathVariable Integer rid){
        Optional<User> userOptional = userService.findUserByUid(uid);
        if(userOptional.isPresent()) {
            if(!userRoleService.findUserHasRole(uid,rid)){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"该用户没有此角色或者角色未激活!!!");
            }else {
                switch (rid) {
                    case RoleType.ADMIN_ROLE:
                        UserAdminVoGet userAdminVoGet = DozerUtil.map(userOptional.get(), UserAdminVoGet.class);
                        return AjaxResponse.success(userAdminVoGet);
                    case RoleType.EDITOR_ROLE:
                        UserEditorVoGet userEditorVoGets = DozerUtil.map(userOptional.get(), UserEditorVoGet.class);
                        return AjaxResponse.success(userEditorVoGets);
                    case RoleType.REVIEWER_ROLE:
                        UserReviewerVoGet userReviewerVoGet = DozerUtil.map(userOptional.get(), UserReviewerVoGet.class);
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

    @GetMapping("/users")
    @PreAuthorize("hasAnyRole('ROLE_1')")
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
                    return AjaxResponse.success(userAdminVoGets);
                case RoleType.EDITOR_ROLE:
                    List<UserEditorVoGet> userEditorVoGets = DozerUtil.mapList(userList,UserEditorVoGet.class);
                    pageData = new PageData(userPage.getTotalPages(), (int) userPage.getTotalElements(),page,userEditorVoGets.size(),userEditorVoGets);
                    return AjaxResponse.success(pageData);
                case RoleType.REVIEWER_ROLE:
                    List<UserReviewerVoGet> userReviewerVoGets = DozerUtil.mapList(userList,UserReviewerVoGet.class);
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

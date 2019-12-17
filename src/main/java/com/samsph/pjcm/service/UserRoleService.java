package com.samsph.pjcm.service;

import com.samsph.pjcm.model.UserRole;
import org.springframework.security.core.authority.SimpleGrantedAuthority;


import java.util.List;

public interface UserRoleService {
     /**
      * 添加用户角色
      * @param userRole 用户角色实体
      */
     void addUserRole(UserRole userRole);

     /**
      *更新用户角色
      * @param userRole 用户角色实体
      */
     void updateUserRole(UserRole userRole);

     /**
      *删除用户角色
      * @param id 用户角色id
      */
     void deleteUserRole(int id);

     /**
      *根据用户角色id得到用户角色
      * @param id 用户角色id
      * @return 用户角色
      */
     UserRole findUserRole(int id);

     /**
      *根据用户id得到用户角色列表
      * @param uid 用户id
      * @return 用户角色列表
      */
     List<UserRole> findUserRolesByUid(int uid);

     /**
      *根据角色id得到用户角色列表
      * @param role 角色id
      * @return 用户角色列表
      */
     List<UserRole> findUserRolesByRole(int role);

     /**
      *根据用户id和角色id判断用户是否有对应角色
      * @param uid 用户id
      * @param role 角色id
      * @return 用户是否有对应角色
      */
     boolean findUserHasRole(int uid,int role);

}

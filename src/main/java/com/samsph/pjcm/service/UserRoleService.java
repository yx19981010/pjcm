package com.samsph.pjcm.service;

import com.samsph.pjcm.model.UserRole;
import org.springframework.security.core.authority.SimpleGrantedAuthority;


import java.util.List;

public interface UserRoleService {
     void addUserRole(UserRole userRole);

     void updateUserRole(UserRole userRole);

     void deleteUserRole(int id);

     UserRole findUserRole(int id);

     List<UserRole> findUserRolesByUid(int uid);

     List<UserRole> findUserRolesByRole(int role);

     boolean findUserHasRole(int uid,int role);

}

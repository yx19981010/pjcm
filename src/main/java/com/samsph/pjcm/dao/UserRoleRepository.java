package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.UserRole;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface UserRoleRepository extends JpaRepository<UserRole,Integer> {
    /**
     * 根据用户id得到用户-角色列表
     * @param uid 用户id
     * @return 用户-角色列表
     */
    List<UserRole> findByUid(Integer uid);

    /**
     * 根据角色id得到角色-用户列表
     * @param role 角色id
     * @return 用户-角色列表
     */
    List<UserRole> findByRole(Integer role);

    /**
     * 根据用户id和角色id得到用户-角色表项
     * @param uid 用户id
     * @param role 角色id
     * @return 用户-角色表项
     */
    @Query(value = "select a from UserRole a where a.uid = :uid and a.role = :role")
    Optional<UserRole> findUserHasRole(@Param("uid") int uid,@Param("role") int role);
}

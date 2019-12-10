package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.UserRole;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface UserRoleRepository extends JpaRepository<UserRole,Integer> {
    List<UserRole> findByUid(Integer uid);
    List<UserRole> findByRole(Integer role);

    @Query(value = "select a from UserRole a where a.uid = :uid and a.role = :role")
    Optional<UserRole> findUserHasRole(@Param("uid") int uid,@Param("role") int role);
}

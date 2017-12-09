import scala.annotation.tailrec

object Day5 extends App {

  val inputraw = "0\n1\n0\n0\n1\n-3\n0\n0\n2\n-2\n-6\n-3\n2\n-5\n-6\n-3\n-3\n0\n-8\n-12\n1\n-9\n-12\n-9\n0\n-7\n-17\n-6\n-18\n-7\n-6\n-21\n-28\n-14\n-23\n-14\n-17\n-5\n-35\n-17\n-26\n-14\n1\n-27\n-19\n-40\n-32\n-44\n2\n-14\n-15\n-12\n-35\n0\n-49\n-12\n-7\n-46\n-47\n-32\n-33\n-47\n-7\n-62\n-20\n-35\n-4\n-35\n-8\n-3\n-61\n-38\n-63\n-27\n-33\n-57\n-48\n-66\n-68\n-11\n-61\n-50\n-34\n-31\n-36\n-79\n-49\n-71\n1\n-34\n-65\n-61\n-91\n-12\n-21\n-82\n-85\n-51\n-89\n0\n-83\n-53\n-44\n-7\n1\n-19\n-39\n-27\n-94\n-36\n-31\n-35\n-97\n-45\n-90\n-15\n-106\n-30\n-79\n-18\n-25\n-105\n-30\n-63\n-109\n-32\n-91\n-96\n-87\n-121\n-116\n-103\n-71\n-1\n-113\n-10\n-47\n-109\n-107\n-38\n-66\n-26\n-8\n-38\n-31\n-129\n-42\n-91\n-89\n-107\n-125\n-75\n-118\n-81\n-45\n-111\n-27\n-63\n-106\n-110\n-64\n-63\n-80\n-44\n-33\n-130\n-55\n-90\n-144\n-15\n-132\n-122\n-155\n-122\n-94\n-159\n-5\n-89\n-6\n-97\n-129\n-159\n-15\n-44\n-156\n-124\n-113\n-154\n-95\n-96\n-29\n-121\n-30\n-73\n-118\n-57\n-76\n-141\n-138\n-108\n-185\n-56\n-136\n-161\n-138\n-192\n2\n-126\n-12\n-39\n-60\n-125\n-149\n-193\n-146\n-116\n-101\n-16\n-207\n-122\n-92\n-204\n-42\n-112\n-28\n-93\n-96\n-57\n-136\n-19\n-36\n-107\n-170\n-19\n-20\n-96\n-229\n-59\n-172\n-58\n-89\n-31\n-57\n-223\n-37\n-189\n-43\n-135\n-90\n-150\n-22\n-152\n-243\n-37\n-231\n-112\n-57\n-168\n-30\n-77\n-162\n-181\n-176\n-202\n-138\n-206\n-183\n-190\n-257\n-181\n-47\n-23\n-248\n-114\n-98\n-77\n-143\n-168\n-166\n-30\n-155\n-237\n-51\n-113\n-243\n-41\n-142\n-231\n-139\n-20\n-190\n-262\n-142\n-238\n-200\n-270\n-113\n-35\n-296\n-146\n-205\n-129\n-198\n-68\n-139\n-56\n-196\n-133\n-16\n-229\n-258\n-91\n-63\n-249\n-274\n-156\n-273\n-182\n-166\n-115\n-154\n-296\n-115\n-89\n-120\n-201\n-44\n-287\n-8\n1\n-260\n-297\n-282\n-114\n-323\n-326\n-166\n-241\n-109\n-21\n-236\n-280\n-19\n-80\n-77\n-271\n-292\n-340\n-300\n-206\n-308\n-99\n-156\n-277\n-245\n-132\n-56\n-172\n-53\n-271\n-32\n-5\n-235\n-329\n-1\n-150\n-247\n-268\n-133\n-341\n-221\n-2\n-43\n-229\n-190\n-337\n-40\n-71\n-72\n-149\n-25\n-253\n-44\n-113\n-164\n-370\n-284\n-235\n-9\n-234\n-291\n1\n-152\n-302\n-393\n-47\n-289\n-75\n-140\n-349\n-140\n-353\n-298\n-27\n-292\n-380\n-55\n-62\n-208\n-221\n-41\n-316\n-411\n-367\n-220\n-248\n-59\n-177\n-372\n-55\n-241\n-240\n-140\n-315\n-297\n-42\n-118\n-141\n-70\n-183\n-153\n-30\n-63\n-306\n-110\n-8\n-356\n-80\n-314\n-323\n-41\n-176\n-165\n-41\n-230\n-132\n-222\n-2\n-404\n-38\n-130\n2\n-16\n-141\n-136\n-336\n-245\n-6\n-348\n-172\n-267\n-208\n-291\n-285\n-67\n-219\n-216\n-136\n-325\n-27\n-382\n-242\n-50\n-284\n-149\n-454\n-336\n-346\n-293\n-402\n-76\n-324\n-219\n-336\n-24\n-446\n-123\n-185\n-196\n-295\n-173\n-400\n-137\n-414\n-14\n-104\n-62\n-252\n-17\n-398\n-490\n-440\n-89\n-347\n-101\n-142\n-228\n-301\n-396\n-320\n-52\n-508\n-122\n-436\n-311\n-344\n-240\n-434\n-220\n-197\n-31\n-295\n-44\n-452\n-269\n-430\n-373\n-409\n-438\n-365\n-13\n-241\n-418\n-20\n-24\n-141\n-1\n-148\n-307\n-63\n-423\n-254\n-8\n-438\n-326\n-19\n-135\n-109\n-394\n2\n-398\n-273\n-158\n-453\n-346\n-86\n-431\n-536\n-549\n-379\n-483\n-85\n-476\n-483\n-104\n-87\n-462\n-249\n-540\n-164\n-360\n-100\n-238\n-45\n-390\n-59\n-156\n-248\n-257\n-150\n-164\n-160\n-545\n-520\n-364\n-384\n-237\n-456\n-28\n-366\n-147\n0\n-303\n-583\n-420\n-370\n-299\n-154\n-380\n-188\n-491\n-258\n-598\n-429\n-349\n-333\n-569\n-4\n-556\n-421\n-182\n-441\n-407\n-542\n-364\n-370\n-384\n1\n-529\n-45\n-319\n-395\n-279\n-160\n-575\n-193\n-25\n-565\n-548\n-445\n-266\n-304\n-361\n-348\n-303\n-159\n-39\n-75\n-437\n-608\n-622\n-556\n-108\n-343\n-283\n-68\n-632\n-393\n-68\n-140\n-126\n-531\n-87\n-519\n-334\n-56\n-70\n-275\n-247\n-370\n-439\n-118\n-497\n-630\n-594\n-612\n-541\n-161\n-646\n-397\n-100\n-284\n-313\n0\n-59\n-200\n-601\n-663\n-529\n-676\n-610\n-7\n-228\n-50\n-494\n-382\n-250\n-306\n-274\n-163\n-110\n-375\n-124\n-237\n-98\n-645\n-692\n-495\n-593\n-647\n-178\n-531\n-336\n-697\n-646\n-671\n-633\n-542\n-461\n-200\n-658\n-525\n-389\n-643\n-258\n-329\n-656\n-400\n-692\n-557\n-506\n-594\n-67\n-623\n-113\n-459\n-211\n-713\n-115\n-602\n-131\n-181\n-30\n-227\n-53\n-719\n-631\n-641\n-434\n-552\n-716\n-368\n-19\n-439\n-443\n-552\n-85\n-79\n-449\n-254\n-620\n-474\n-121\n-210\n-285\n-608\n-456\n-513\n-496\n-13\n-418\n-399\n-437\n-258\n-15\n-623\n-178\n-336\n-379\n-721\n-299\n-729\n-742\n-64\n-13\n-438\n-603\n-666\n-278\n-767\n-200\n-686\n-497\n-256\n-541\n-491\n-360\n-615\n-326\n-682\n-759\n-524\n-580\n-323\n-578\n-793\n-478\n-107\n-440\n-657\n-790\n-605\n-21\n-163\n-392\n-560\n-336\n-430\n-613\n-182\n-15\n-782\n-607\n-281\n-269\n-25\n-699\n-89\n-593\n-280\n-269\n-438\n-103\n-359\n-387\n-157\n-747\n-619\n-176\n-772\n-500\n-735\n-691\n-797\n-612\n-573\n-36\n-617\n-630\n-357\n-718\n-210\n-48\n-185\n-20\n-556\n-206\n-722\n-559\n-416\n-578\n-745\n-564\n-273\n-62\n-300\n-218\n-711\n-744\n-805\n-277\n-522\n-346\n-280\n-762\n-438\n-381\n-379\n-198\n-737\n-555\n-466\n-218\n-511\n-334\n-353\n-259\n-225\n-675\n-350\n-585\n-647\n-52\n-395\n-324\n-106\n-826\n-279\n-81\n-396\n-611\n-312\n-529\n-291\n-129\n-594\n-437\n-188\n-649\n-820\n-237\n-673\n-6\n-387\n-195\n-503\n-350\n-83\n-88\n-626\n-30\n-313\n-13\n-633\n-403\n-319\n-832\n-185\n-146\n-839\n-9\n-557\n-799\n-841\n-700\n-465\n-669\n-769\n-235\n-849\n-863\n-819\n-76\n-912\n-931\n-909\n-762\n-607\n-522\n-64\n-769\n-377\n-133\n-414\n-772\n-206\n-746\n-730\n-393\n-901\n-72\n-33\n-811\n-372\n-298\n-835\n-637\n-302\n-481\n-958\n-878\n-867\n-25\n-260\n-448\n-21\n-930\n-903\n-581\n-547\n-664\n-843\n-140\n-337\n-383\n-513\n-368\n-221\n-474\n-169\n-673\n-728\n-266\n-862\n-753\n-815\n-647\n-106\n-15\n-728\n-912\n-147\n-828\n-6\n-694\n-434\n-737\n-335\n-183\n-732\n-841\n-364\n-155\n-116\n-966\n-822\n-65\n-22\n-853\n-208\n-326\n-826\n-472\n-491\n-436\n-771\n-1009\n-98\n-401\n-915\n-275\n-574\n-313\n-884\n-648\n-935\n-94\n-326\n-553\n-744\n-723\n-782\n-719\n-175\n-868\n-190\n-153\n-48\n-218\n-414\n-721\n-715\n-995\n-991\n-575\n-264\n-70\n-366\n-381\n-130\n-409\n-817\n-258\n-1028\n-552\n-878\n-449\n-138\n-900\n-45\n-119\n-677\n-844\n-869\n-985\n-1019\n-60\n-649\n-915\n-93\n-1053\n-121\n-631\n-156\n-332\n-193"
  val offsets = inputraw.split("\n").toList.map(_.toInt)
//  var offsets = List(0, 3,  0,  1,  -3)

  println(offsets)

  @tailrec
  def cycle(step: Int, pos: Int, offsets: List[Int]): Unit = {
//    print(s"steps: %s pos: %s \n".format(step, pos))

    if (pos < 0 ||  pos > offsets.size-1) { print("finished, steps: "+step); System.exit(0) };

    val newpos = pos + offsets(pos)

    cycle(step+1, newpos, offsets.updated(pos, offsets(pos)+1))
  }


  cycle(0, 0, offsets)


}